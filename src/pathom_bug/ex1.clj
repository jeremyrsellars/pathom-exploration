(ns pathom-bug.ex1
  "Resolvers to fetch language-specific titles for a 2-level hierarchy
   where the titles at both levels are fetched in a single batch."
  (:require clojure.pprint
            [com.wsscode.misc.coll :as coll]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom.viz.ws-connector.pathom3 :as p.connector]
            [com.wsscode.pathom.viz.ws-connector.core :as pvc]))

(def mock-languages-db
  [{:language/id 1, :language/code "en"}
   {:language/id 2, :language/code "es"}])

(def mock-resource-db
  [{:language/id 1, :language/code :language/en, :lang-resource/rid 1, :lang-resource/rid-code "en-1", :lang-resource/value "en:1"}
   {:language/id 2, :language/code :language/es, :lang-resource/rid 1, :lang-resource/rid-code "es-1", :lang-resource/value "es:1"}
   {:language/id 1, :language/code :language/en, :lang-resource/rid 2, :lang-resource/rid-code "en-2", :lang-resource/value "en:2"}
   {:language/id 2, :language/code :language/es, :lang-resource/rid 2, :lang-resource/rid-code "es-2", :lang-resource/value "es:2"}
   {:language/id 1, :language/code :language/en, :lang-resource/rid 3, :lang-resource/rid-code "en-3", :lang-resource/value "en:3"}
   {:language/id 2, :language/code :language/es, :lang-resource/rid 3, :lang-resource/rid-code "es-3", :lang-resource/value "es:3"}
   {:language/id 1, :language/code :language/en, :lang-resource/rid 4, :lang-resource/rid-code "en-4", :lang-resource/value "en:4"}
   {:language/id 2, :language/code :language/es, :lang-resource/rid 4, :lang-resource/rid-code "es-4", :lang-resource/value "es:4"}])

(def mock-parents-db
  [{:parent/id 1, :parent/title-rid 1}])

(pco/defresolver parent-title-resource
  [_env input]
  {::pco/input   [:parent/title-rid]
   ::pco/output  [{:parent/title-resource [:lang-resource/rid]}]}
  {:parent/title-resource
   {:lang-resource/rid (:parent/title-rid input)}})

(pco/defresolver child-title-resource
  [_env input]
  {::pco/input   [:child/title-rid]
   ::pco/output  [{:child/title-resource [:lang-resource/rid]}]}
  {:child/title-resource
   {:lang-resource/rid (:child/title-rid input)}})

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

(pco/defresolver parents-by-ids-batched
  "Fetches specific child parents by parent/id"
  [_env items]
  {::pco/input  [:parent/id]
   ::pco/output [{:parent parent-attributes}]
   ::pco/batch? true}
  (->> (map :parent/id items)
       mock-parents-db
       (coll/restore-order items :parent/id)
       (hash-map :parent)))

(pco/defresolver parents-by-ids-one
  "Fetches specific child parents by parent/id"
  [_env items]
  {::pco/input  [:parent/id]
   ::pco/output [{:parent parent-attributes}]}
  (->> (map :parent/id items)
       mock-parents-db
       (coll/restore-order items :parent/id)
       (hash-map :parent)))

;; children

(def child-attributes
  [:child/id
   :parent/id
   :child/title-rid])

(defn lookup-children-by-parent-id
  [id]
  {:parent/children
   [{:child/id (+ 100 id)
     :parent/id id
     :child/title-rid 1}
    {:child/id (+ 101 id)
     :parent/id id
     :child/title-rid 3}]})

(pco/defresolver children-in-parents-batched
  "Fetches children in child parents specified by parent/id"
  [_env parents]
  {::pco/input  [:parent/id]
   ::pco/output [{:parent/children child-attributes}]
   ::pco/batch? true}
  (let [parent-ids (map :parent/id parents)]
    (mapv lookup-children-by-parent-id
          parent-ids)))

(pco/defresolver children-in-parents-one
  "Fetches children in child parents specified by parent/id"
  [_env {:keys [parent/id]}]
  {::pco/input  [:parent/id]
   ::pco/output [{:parent/children child-attributes}]}
  (lookup-children-by-parent-id id))

(def resolvers
  (let [show-batch-bug? true]
    [parent-title-resource
     child-title-resource
     (if true resource-value-batched resource-value-one)
     resource-values
     parents
     (if show-batch-bug?
       children-in-parents-batched
       children-in-parents-one)
     #_|]))

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

(defn run
  "clj -X pathom-bug.ex1/run"
  [_opts]
  (doc-example query)
  (System/exit 0))

(comment
  (doc-example query)
  #_|)

(defn -main []
  (doc-example query))
