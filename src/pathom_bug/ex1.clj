(ns pathom-bug.ex1
  "Resolvers to fetch child catalog information from the database."
  (:require clojure.pprint
            [clojure.string :as string]
            [com.wsscode.misc.coll :as coll]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom.viz.ws-connector.pathom3 :as p.connector]
            [com.wsscode.pathom.viz.ws-connector.core :as pvc]))

(def language-attributes
  [:language/id
   :language/code])

(def mock-languages-db
  [{:language/id 1, :language/code "en"}
   {:language/id 2, :language/code "es"}])

(def mock-resource-db
  (into []
        (for [rid (range 100)
              {:keys [language/id language/code] :as l} mock-languages-db]
          (assoc l
                 :language/id id
                 :language/code (keyword "language" (name code))
                 :lang-resource/rid rid
                 :lang-resource/rid-code (str (name code) "-" rid)
                 :lang-resource/value (str (name code) ":" rid)))))

(def mock-parents-db
  [{:parent/id 1, :parent/title-rid 1}])

(pco/defresolver languages
  "Fetches all languages"
  [_ _]
  {::pco/output [{:languages language-attributes}]}
  {:languages mock-languages-db})

(pco/defresolver languages-by-ids
  "Fetches specific child languages by parent/id"
  [_env items]
  {::pco/input  [:language/id]
   ::pco/output language-attributes
   ::pco/batch? true}
  (let [ids (map :language/id items)]
    (->> ids
         (mapv #(some (fn [{:keys [:language/id]}] (when (= id %) %)) mock-languages-db)))))

(defn resource-seed-resolver
  "Creates a resolver for the specified resource id attribute
   that resolves to a map containing :lang-resource/rid
   (a seed of the resource onto which languages can be queried)
   without assigning to a var."
  ([resource-id-attr-kw]
   (resource-seed-resolver resource-id-attr-kw
                           (-> resource-id-attr-kw
                               (.-sym)
                               (string/replace #"[-_]rid$" "-resource")
                               keyword)))
  ([resource-id-attr-kw resource-attr-kw]
   (let [sym (symbol (str (ns-name *ns*))
                     (-> resource-attr-kw
                         (.-sym) ; remove keyword colon
                         str
                         (string/replace #"(?i)[^-_A-Z0-9]" "__")))]
     (pco/resolver {::pco/op-name sym
                    ::pco/input   [resource-id-attr-kw]
                    ::pco/output  [{resource-attr-kw [:lang-resource/rid]}]
                    ::pco/resolve (fn resource-id-resolver
                                    [_env input]
                                    {resource-attr-kw
                                     {:lang-resource/rid (resource-id-attr-kw input)}})}))))

(pco/defresolver resource-values
  "Expands from a resource id to a list of language-specific resources based on a known set of languages.
   The languages may be specified by EQL query parameter, like
   `({:lang-resource/resources [:language/code :lang-resource/value]}
     {:language/codes [:language/en :language/es]})
   Or the languages are fetched from the data store."
  [env {:keys [:lang-resource/rid]}]
  {::pco/input  [:lang-resource/rid]
   ::pco/output [{:lang-resource/resources [:lang-resource/rid-code :language/code]}]}
  (let [lang-codes (or (some->> env pco/params :language/codes)
                       (map (fn [{:keys [language/code]}]
                              (keyword "language" (name code)))
                            mock-languages-db))]
    {:lang-resource/resources
     (mapv #(hash-map :lang-resource/rid-code (str (name %) "-" rid), :language/code %)
           lang-codes)}))

(pco/defresolver resource-value-batched
  "Resolves language-specific resource values.
   This is a batched operation that fetches all the resources for all the specified languages,
   which might be overkill."
  [_env resources]
  {::pco/input  [:lang-resource/rid-code]
   ::pco/output [:lang-resource/value]
   ::pco/batch? true}
  (let [indexed (zipmap (map :lang-resource/rid-code mock-resource-db)
                        mock-resource-db)]
    (mapv (fn [{:keys [lang-resource/rid-code]}]
            {:lang-resource/value (get indexed rid-code)})
          resources)))

(pco/defresolver resource-value-one
  "Resolves language-specific resource values.
   This is a batched operation that fetches all the resources for all the specified languages,
   which might be overkill."
  [_env resource]
  {::pco/input  [:lang-resource/rid-code]
   ::pco/output [:lang-resource/value]}
  (let [items mock-resource-db
        indexed (zipmap (map :lang-resource/rid-code items)
                        items)]
    (some (fn [{:keys [lang-resource/rid-code]}]
            {:lang-resource/value (get indexed rid-code)})
          [resource])))

;; parents

(def parent-attributes
  [:parent/id
   :parent/title-rid])

(pco/defresolver parents
  "Fetches all child parents"
  [_env _]
  {::pco/output [{:child-parents parent-attributes}]}
  {:child-parents (vec mock-parents-db)})

(pco/defresolver parents-by-ids
  "Fetches specific child parents by parent/id"
  [_env items]
  {::pco/input  [:parent/id]
   ::pco/output [{:parent parent-attributes}]
   ::pco/batch? true}
  (->> (map :parent/id items)
       mock-parents-db
       (coll/restore-order items :parent/id)
       (hash-map :parent)))

;; children

(def child-attributes
  [:child/id
   :child/key
   :parent/id
   :child/title-rid])

(pco/defresolver children-in-parents
  "Fetches children in child parents specified by parent/id"
  [_env parents]
  {::pco/input  [:parent/id]
   ::pco/output [{:parent/children child-attributes}]
   ::pco/batch? true}
  (let [parent-ids (map :parent/id parents)]
    (mapv (fn [id] {:parent/children
                    [{:child/id (+ 100 id)
                      :child/key (keyword (str "child-" (+ 100 id)))
                      :parent/id id
                      :child/title-rid 1}
                     {:child/id (+ 101 id)
                      :child/key (keyword (str "child-" (+ 101 id)))
                      :parent/id id
                      :child/title-rid 3}]})
          parent-ids)))


(def rid-kws
  (->> (concat language-attributes
               parent-attributes
               child-attributes)
       (into []
             (filter #(string/ends-with? (.-sym %) "-rid")))))

(def resolvers
  (-> (mapv resource-seed-resolver rid-kws)
      (conj languages
            languages-by-ids
            (if true resource-value-batched resource-value-one)
            resource-values
            parents
            parents-by-ids
            children-in-parents
            #_|)))

(def env
  (cond-> (merge {:db/sql {} #_sql/sql} (pci/register resolvers))
    :connect-parser
    (p.connector/connect-env {::pvc/parser-id `env})))

(def query
  [{:child-parents
    [:parent/id
     {:parent/title-resource [:lang-resource/rid {:lang-resource/resources [:language/code :lang-resource/value]}]}
     {:parent/children
      [:child/id
       :child/key
       :child/title-rid
       {:child/title-resource [:lang-resource/rid {:lang-resource/resources [:language/code :lang-resource/value]}]}
       #_|]}]}])

(defn doc-example
  [edn-query]
  (println "query")
  (println "---------------")
  (println)
  (println "```edn")
  (clojure.pprint/pprint query)
  (println "```")
  (println "result")
  (println "---------------")
  (println "```edn")
  (clojure.pprint/pprint (p.eql/process env edn-query))
  (println "```"))

(defn run [_opts]
  (doc-example query)
  (System/exit 0))
