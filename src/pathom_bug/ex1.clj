(ns pathom-bug.ex1
  "Resolvers to fetch course catalog information from the database."
  (:require clojure.pprint
            [clojure.string :as string]
            [com.wsscode.misc.coll :as coll]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom.viz.ws-connector.pathom3 :as p.connector]))

(def language-attributes
  [:language/id
   :language/code])

(def data-languages
  [{:language/id 1, :language/code "en"}
   {:language/id 2, :language/code "es"}])

(def mock-resource-db
  (into []
        (for [rid (range 100)
              {:keys [language/id language/code] :as l} data-languages]
          (assoc l
                 :language/id id
                 :language/code (keyword "language" (name code))
                 :lang-resource/rid rid
                 :lang-resource/rid-code (str (name code) "-" rid)
                 :lang-resource/value (str (name code) ":" rid)))))

(def data-categories
  [{:course-category/id 1, :course-category/title-rid 1}])

(pco/defresolver languages
  "Fetches all languages"
  [_ _]
  {::pco/output [{:languages language-attributes}]}
  {:languages data-languages})

(pco/defresolver languages-by-ids
  "Fetches specific course languages by course-category/id"
  [_env items]
  {::pco/input  [:language/id]
   ::pco/output language-attributes
   ::pco/batch? true}
  (let [ids (map :language/id items)]
    (->> ids
         (mapv #(some (fn [{:keys [:language/id]}] (when (= id %) %)) data-languages)))))

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
                                    (prn ::resource-seed-resolver resource-id-attr-kw resource-attr-kw #_input)
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
                       (prn "Warning: fetching languages for resource-values")
                       (map (fn [{:keys [language/code]}]
                              (keyword "language" (name code)))
                            data-languages))]
    ;; (prn 'resource-values lang-codes 'for-rid rid #_#_:env env :params (pco/params env))
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

;; Categories

(def category-attributes
  [:course-category/id
  ;;  :course-category/key
   :course-category/title-rid])
  ;;  :course-category/order
  ;;  :course-category/short-description-rid
  ;;  :course-category/beta?
  ;;  :course-category/new?
  ;;  :course-category/thumb-src
  ;;  :course-category/title-screen-src
  ;;  :course-category/overlay-rid
  ;;  :course-category/long-description-rid])

(pco/defresolver categories
  "Fetches all course categories"
  [_env _]
  {::pco/output [{:course-categories category-attributes}]}
  ;(prn :categories (store/get-categories sql))
  {:course-categories (vec data-categories)}) ;(store/get-categories sql))})

(comment
  ((:resolve categories) {:db/sql sql/sql} nil)
  |)

(pco/defresolver categories-by-ids
  "Fetches specific course categories by course-category/id"
  [_env items]
  {::pco/input  [:course-category/id]
   ::pco/output [{:course-category category-attributes}]
   ::pco/batch? true}
  (->> (map :course-category/id items)
       data-categories
       (coll/restore-order items :course-category/id)
       (hash-map :course-category)))

(comment
  ((:resolve categories-by-ids) {:db/sql sql/sql} [{:course-category/id 2} {:course-category/id 1}])
  |)

;; Courses

(def course-attributes
  [:course/id
   :course/key
   :course-category/id
   :course/title-rid])
  ;;  :course/short-description-rid
  ;;  :course/long-description-rid
  ;;  :course/overlay-rid
  ;;  :course/play-time-rid
  ;;  :course/order
  ;;  :course/beta?
  ;;  :course/new?
  ;;  :course/image])

(def activity-attributes
  (empty
   [:course/id
    :course-activity/id
    :course-activity/story-name
    :course-activity/type
    :course-activity/order
    :course-activity/title-rid
    :course-activity/short-title-rid
    :course-activity/description-rid
    :course-activity/image
    :course-activity/new?
    :course-activity/beta?]))

(def category-resource-attributes
  (empty
   [:course/id
    :category-resource/key
    :category-resource/type
    :category-resource/order
    :language/id
    :category-resource/title-rid
    :category-resource/description-rid
    :category-resource/cover-image-src]))

(def attachment-section-attributes
  (empty
   [:course-category/id
    :category-attachment-section/id
    :category-attachment-section/title-rid
    :category-attachment-section/order]))

(def attachment-attributes
  (empty
   [:category-attachment-section/id
    :category-attachment/id
    :category-attachment/title-rid
    :category-attachment/description-rid
    :category-attachment/link-rid
    :category-attachment/cover-image-src
    :category-attachment/order]))

;; (pco/defresolver courses
;;   "Fetches all courses"
;;   [_env _]
;;   {::pco/output course-attributes}
;;   (store/get-courses sql))

;; (comment
;;   ((:resolve courses) {:db/sql sql/sql} nil)
;;   |)

;; (pco/defresolver courses-by-ids
;;   "Fetches specific courses by course/id"
;;   [_env items]
;;   {::pco/input  [:course/id]
;;    ::pco/output course-attributes
;;    ::pco/batch? true}
;;   (->> (map :course/id items)
;;        (store/get-courses sql)
;;        (coll/restore-order items :course/id)))

;; (comment
;;   ((:resolve courses-by-ids) {:db/sql sql/sql} [{:course/id 2}{:course/id 1}])
;;   |)

(pco/defresolver courses-in-categories
  "Fetches courses in course categories specified by course-category/id"
  [_env categories]
  {::pco/input  [:course-category/id]
   ::pco/output [{:course-category/courses course-attributes}]
   ::pco/batch? true}
  (let [category-ids (map :course-category/id categories)]
    (mapv (fn [id] {:course-category/courses
                    [{:course/id (+ 100 id)
                      :course/key (keyword (str "course-" (+ 100 id)))
                      :course-category/id id
                      :course/title-rid 1}
                     {:course/id (+ 101 id)
                      :course/key (keyword (str "course-" (+ 101 id)))
                      :course-category/id id
                      :course/title-rid 3}]})
          category-ids)))

(pco/defresolver course-activities
  "Fetches all activities"
  [_env _]
  {::pco/output [{:course-catalog/activities activity-attributes}]}
  {:course-catalog/activities []});(store/get-activities sql)})

;; (pco/defresolver course-activities-by-ids
;;   "Fetches activities by course-activity/id"
;;   [_env course-activities]
;;   {::pco/input  [:course-activity/id]
;;    ::pco/output activity-attributes
;;    ::pco/batch? true}
;;   (let [activity-ids (map :course-activity/id course-activities)]
;;     (->> (store/get-activities sql activity-ids)
;;          (coll/restore-order2 activity-ids :course-activity/id))))

(pco/defresolver course-activities-in-courses
  "Fetches activities in courses specified by course/id"
  [_env courses]
  {::pco/input  [:course/id]
   ::pco/output [{:course/activities activity-attributes}]
   ::pco/batch? true}
  (let [course-ids (map :course/id courses)]
        ;; grouped
        ;; (->> course-ids
        ;;      (store/get-activities-in-courses sql)
        ;;      (group-by :course/id))]
    (mapv (fn [id] {:course/activities [] #_(grouped id)})
          course-ids)))

(pco/defresolver course-library-resources
  "Fetches all resources"
  [_env _]
  {::pco/output [{:course-catalog/resources category-resource-attributes}]}
  {:course-catalog/resources []});(store/get-library-resources sql)})

;; (pco/defresolver course-library-resources-by-ids
;;   "Fetches resources by category-resource/id"
;;   [_env course-library-resources]
;;   {::pco/input  [:category-resource/id]
;;    ::pco/output category-resource-attributes
;;    ::pco/batch? true}
;;   (let [resource-ids (map :category-resource/id course-library-resources)]
;;     (->> (store/get-library-resources sql resource-ids)
;;          (coll/restore-order2 resource-ids :category-resource/id))))

(pco/defresolver course-library-resources-in-categories
  "Fetches resources in courses specified by :course-category/id"
  [_env categories]
  {::pco/input  [:course-category/id]
   ::pco/output [{:course-category/resources category-resource-attributes}]
   ::pco/batch? true}
  (let [category-ids (map :course-category/id categories)]
        ;; grouped
        ;; (->> category-ids
        ;;      (store/get-library-resources-in-categories sql)
        ;;      (group-by :course-category/id))]
    (mapv (fn [id] {:course-category/resources []});(grouped id)})
          category-ids)))

;; (pco/defresolver category-attachment-sections
;;   "Fetches all attachment-sections"
;;   [_env _]
;;   {::pco/output [{:course-catalog/attachment-sections attachment-section-attributes}]}
;;   {:course-catalog/attachment-sections (store/get-attachment-sections sql)})

;; (pco/defresolver category-attachment-sections-by-ids
;;   "Fetches attachment-sections by category-attachment-section/id"
;;   [_env category-attachment-sections]
;;   {::pco/input  [:category-attachment-section/id]
;;    ::pco/output attachment-section-attributes
;;    ::pco/batch? true}
;;   (let [attachment-section-ids (map :category-attachment-section/id category-attachment-sections)]
;;     (->> (store/get-attachment-sections sql attachment-section-ids)
;;          (coll/restore-order2 attachment-section-ids :category-attachment-section/id))))

;; (pco/defresolver category-attachment-sections-in-categories
;;   "Fetches attachment-sections in categories specified by course/id"
;;   [_env categories]
;;   {::pco/input  [:course-category/id]
;;    ::pco/output [{:course-category/attachment-sections attachment-section-attributes}]
;;    ::pco/batch? true}
;;   (let [category-ids (map :course-category/id categories)
;;         grouped
;;         (->> category-ids
;;              (store/get-attachment-sections-in-categories sql)
;;              (group-by :course-category/id))]
;;     (map (fn [id] {:course-category/attachment-sections (grouped id)})
;;          category-ids)))

;; (pco/defresolver category-attachments
;;   "Fetches all attachments"
;;   [_env _]
;;   {::pco/output [{:course-catalog/attachments attachment-attributes}]}
;;   {:course-catalog/attachments (store/get-attachments sql)})

;; (pco/defresolver category-attachments-by-ids
;;   "Fetches attachments by category-attachment/id"
;;   [_env category-attachments]
;;   {::pco/input  [:category-attachment/id]
;;    ::pco/output attachment-attributes
;;    ::pco/batch? true}
;;   (let [attachment-ids (map :category-attachment/id category-attachments)]
;;     (->> (store/get-attachments sql attachment-ids)
;;          (coll/restore-order2 attachment-ids :category-attachment/id))))

;; (pco/defresolver category-attachments-in-sections
;;   "Fetches attachments in category-attachment-sections specified by category-attachment-section/id"
;;   [_env category-attachment-sections]
;;   {::pco/input  [:category-attachment-section/id]
;;    ::pco/output [{:category-attachment-section/attachments attachment-attributes}]
;;    ::pco/batch? true}
;;   (let [course-ids (map :category-attachment-section/id category-attachment-sections)
;;         grouped
;;         (->> course-ids
;;              (store/get-attachments-in-sections sql)
;;              (group-by :category-attachment-section/id))]
;;     (map (fn [id] {:category-attachment-section/attachments (grouped id)})
;;          course-ids)))

(def rid-kws
  (->> (concat language-attributes
               category-attributes
               course-attributes
               activity-attributes
               category-resource-attributes
               attachment-section-attributes
               attachment-attributes)
       (remove #{});:course-category/title-rid})
      ;;            :course/title-rid})
       (into []
             (filter #(string/ends-with? (.-sym %) "-rid")))))

;; [(resource-seed-resolver :course-category/title-rid)
;;  course-category__title-resource_defresolver]

(def resolvers
  (-> (mapv resource-seed-resolver rid-kws)
      ;; (conj course__title-resource_defresolver
      ;;       course-category__title-resource_defresolver)
      ;(into (map resource-value-resolver [:language/en :language/es]))
      ;[]
      (conj languages
            languages-by-ids
            (if true resource-value-batched resource-value-one)
            resource-values
            categories
            categories-by-ids
            ;; courses
            ;; courses-by-ids
            courses-in-categories
            ;; course-activities
            ;; course-activities-by-ids
            ;; course-activities-in-courses
            ;; course-library-resources
            ;; course-library-resources-by-ids
            ;; course-library-resources-in-categories
            ;; category-attachment-sections
            ;; category-attachment-sections-by-ids
            ;; category-attachment-sections-in-categories
            ;; category-attachments
            ;; category-attachments-by-ids
            ;; category-attachments-in-sections
            #_|)))

(require '[com.wsscode.pathom3.connect.indexes :as pci] '[com.wsscode.pathom3.interface.eql :as p.eql] #_'[banzai.pathom.interface :as p])
(require '[com.wsscode.pathom.viz.ws-connector.core :as pvc] '[com.wsscode.pathom.viz.ws-connector.pathom3 :as p.connector])
(def env
  (cond-> (merge {:db/sql {} #_sql/sql} (pci/register resolvers))
    :connect-parser
    (p.connector/connect-env {::pvc/parser-id `env})))


;; (p.eql/process
;;   env
;;   {:list [{:lang-resource/rid 3}
;;           {:lang-resource/rid 4}]}
;;   [{:list [:language/en :language/es]}])

;; (p.eql/process
;;   env
;;   {:list [{:lang-resource/rid 3 :language/code "en"}
;;           {:lang-resource/rid 3 :language/code "es"}
;;           {:lang-resource/rid 4 :language/code "en"}]}
;;   [{:list [:lang-resource/rid :language/code :lang-resource/value]}])

(let [resource-fragment [;:lang-resource/rid-code ;:lang-resource/resources
                         `({:lang-resource/resources [:language/code :lang-resource/value #_:lang-resource/rid #_:lang-resource/rid-code]}
                           {:language/codes [:language/en :language/es]})]]
  (as->
   (p.eql/process
    env
  ;;  {:list [#_{:course-category/id 2}{:course-category/id 1}#_{:course-category/id 3}]}
    [{:course-categories
      [;:course-category/id
       ;{:course-category/title-resource resource-fragment}
       ;:course-category/title-rid;{:course-category/title [:lang-resource/resources #_[:lang-resource/rid :language/code :lang-resource/value]]}
       :course-category/id
      ;;  :course-category/key
      ;;  :course-category/beta? ;isBeta
      ;;  :course-category/new? ;isNew
       {:course-category/title-resource resource-fragment}
      ;;  {:course-category/short-description-resource resource-fragment} ;shortDescription
      ;;  {:course-category/long-description-resource resource-fragment}  ;longDescription
      ;;  :course-category/title-screen-src
      ;;  :course-category/thumb-src
      ;;  {:course-category/overlay-resource resource-fragment}
       {:course-category/courses [:course/id
                                  :course/key
                                  ;; :course-category/id
                                  ;; :course/order
                                  ;; :course/beta? ;isBeta
                                  ;; :course/new? ;isNew
                                  ;; :course/image
                                  ;; :course/title-rid
                                  ;; {:course/title-resource resource-fragment}:course/title-rid
                                  ;; :course/title-resource
                                  ;; {:course/title-resource [:lang-resource/resources]}
                                  {:course/title-resource resource-fragment}
                                ;;  {:lang-resource/resources [:language/code :lang-resource/value :lang-resource/rid]}

                                  ;; {:course/short-description-resource resource-fragment}:course/short-description-rid]}]}]))
                                  ;#_{:course/long-description-resource resource-fragment}:course/long-description-rid
                                  ;#_{:course/overlay-resource resource-fragment}:course/overlay-rid
                                  ; insert activities
                                  ;#_{:course/play-time-resource resource-fragment}]}:course/play-time-rid
                                  #_|]}]}])
      ;;  {:course-category/attachment-sections [;:category-attachment-section/id
      ;;                                         ;:category-attachment-section/order
      ;;                                         {:category-attachment-section/title-resource resource-fragment}
      ;;                                         {:category-attachment-section/attachments [;:category-attachment/id
      ;;                                                                                    {:category-attachment/title-resource resource-fragment}
      ;;                                                                                    {:category-attachment/description-resource resource-fragment}
      ;;                                                                                    {:category-attachment/link-resource resource-fragment}]}]}
      ;;                                                                                    ;:category-attachment/cover-image-src]}]}
      ;;                                                                                    ;:category-attachment/order]]}
      ;;  {:course-category/resources [:category-resource/key ; library
      ;;                               :category-resource/type
      ;;                               :language/code
      ;;                               :category-resource/cover-image-src
      ;;                               :category-resource/order
      ;;                               {:category-resource/title-resource resource-fragment}
      ;;                               {:category-resource/description-resource resource-fragment}]}]}]))
   result
    {:result result
     :meta (meta result)}))

;; (p.eql/process
;;   env
;;   {:list [#_{:course-category/id 2}{:course-category/id 1}#_{:course-category/id 3}]}
;;   [{:list [:course-category/id
;;            :course-category/title-rid;{:course-category/title [:lang-resource/resources #_[:lang-resource/rid :language/code :lang-resource/value]]}
;;            {:course-category/courses
;;             [:course/id
;;              {:course-category/attachment-sections [:category-attachment-section/id]}]}]}])

;; (p.eql/process
;;   env
;;   {:list [{:lang-resource/rid 3 :language/code "en" :lang-resource/value "hello"}
;;           {:lang-resource/rid 3 :language/code "es" :lang-resource/value "hola"}
;;           {:lang-resource/rid 4 :language/code "en" :lang-resource/value "hi"}]}
;;   [{:list [:language/en :language/es]}])

;; (p.eql/process
;;   env
;;   [:course-catalog/resources
;;    :course-catalog/attachments])
#_(p.eql/process
   env
   {:list [{:course/id 4} {:course/id 1}]}
   [{:list [:course-category/id
            :course/id
            :course/new?
            {:course/activities [:course-activity/id
                                 :course-activity/image]}
            {:course/resources [:category-resource/key
                                :category-resource/cover-image-src]}
            :course-category/attachment-sections]}])

#_(p.eql/process
   env
   {:list [#_{:course-category/id 2} {:course-category/id 1} #_{:course-category/id 3}]}
   [{:list [:course-category/id
            {:course-category/courses
             [:course/id
              {:course-category/attachment-sections [:category-attachment-section/id]}]}]}])
#_(p.eql/process
   env
   {:list [#_{:course-category/id 2} {:course-category/id 1} #_{:course-category/id 3}]}
   [{:list [:course-category/id ;:course-category/order
            {:course-category/courses
             [:course/id
              :course/new?
              {:course/activities [:course-activity/id
                                   :course-activity/image]}
              {:course/resources [:category-resource/key
                                  :category-resource/cover-image-src
                                  :category-resource/type
                                  :category-resource/order
                                  :category-resource/title-rid
                                  :category-resource/description-rid]}
             ;:course-category/attachment-sections #_
              {:course-category/attachment-sections [:category-attachment-section/id
                                                     :category-attachment-section/order
                                                     :category-attachment-section/title-rid
                                                     :category-attachment-section/attachments]}]}]}])
                                           ;                                         [:category-attachment/id]]}]}]}])
                                                                                    ;; :attachment/key
                                                                                    ;; :attachment/order
                                                                                    ;; :attachment/title-rid
                                                                                    ;; :attachment/link-rid
                                                                                    ;; :attachment/cover-image-src]}]}]}]}])
#_(p.eql/process
   env
   {:list [#_{:course-category/id 2} {:course-category/id 1} #_{:course-category/id 3}]}
   [{:course-categories
     [:course-category/id
      :course-category/order
      {:course-category/title [:language/en :language/es :lang-resource/rid]}]}])
    ;; :language/en]}])
#_(comment
    #_[:course-category/id ;:course-category/order #_
  ;; :course-category/courses
       #_{:course-category/courses
          [:course/id
           :course/new?
           {:course/activities [:course-activity/id
                                :course-activity/image]}
           {:course/resources [:category-resource/key
                               :category-resource/cover-image-src
                               :category-resource/type
                               :category-resource/order
                               :category-resource/title-rid
                               :category-resource/description-rid]}
             ;:course-category/attachment-sections #_
           {:course-category/attachment-sections [:category-attachment-section/id
                                                  :category-attachment-section/order
                                                  :category-attachment-section/title-rid
                                                  :category-attachment-section/attachments]}]}])
                                           ;                                         [:category-attachment/id]]}]}]}])
                                                                                    ;; :attachment/key
                                                                                    ;; :attachment/order
                                                                                    ;; :attachment/title-rid
                                                                                    ;; :attachment/link-rid
                                                                                    ;; :attachment/cover-image-src]}]}]}]}])

#_(comment
    (store/get-courses-in-categories sql/sql [2 1])
    ((:resolve courses-in-categories) {:db/sql sql/sql} [{:course-category/id 2} {:course-category/id 1}])
    |

    (require '[com.wsscode.pathom3.connect.indexes :as pci] '[com.wsscode.pathom3.interface.eql :as p.eql])
  ;; (def env
  ;;   (pci/register
  ;;       categories))
  ;;       ;; categories-by-ids
  ;;       ;; courses
  ;;       ;; courses-by-ids
  ;;       ;; courses-in-categories]))
  ;; (def pathom (p.eql/boundary-interface (assoc (pci/register resolvers):db/sql sql/sql)))
  ;(p.eql/process
    (def env {:db/sql sql/sql})
    ((p.eql/boundary-interface (pci/register categories))
     {:db/sql sql/sql}
     [:course-categories])
    ((p.eql/boundary-interface (pci/register course-activities))
     {:db/sql sql/sql}
     [:course/activities])
    (;(p.eql/boundary-interface)
   ; (pci/register resolvers);(p.eql/boundary-interface (pci/register resolvers))
     pathom
  ;;  {:db/sql        sql/sql
  ;;   :pathom/entity [{:course-category/id 2}]}
     {:course-category/id 2}
     [:course-categories])


    (require '[com.wsscode.pathom3.connect.indexes :as pci] '[com.wsscode.pathom3.interface.eql :as p.eql] #_'[banzai.pathom.interface :as p])
    (def env (merge {:db/sql sql/sql} (pci/register resolvers)))


    (p.eql/process
     env
     {:list [{:course-category/id 2} {:course-category/id 1}]}
     [{:list [:course-category/courses :course-category/id]}])

    (p.eql/process
     env
    ;; {:list [{:course-category/id 2}{:course-category/id 1}]}
     [{:course/activities [:course/id :course-activity/id]}])

    (p.eql/process
     env
     {:list [{:course-activity/id 2} {:course-activity/id 1}]}
     [{:course/activities [:course/id :course-activity/id]}])


  ;; Activity
    (p.eql/process
     env
     [:course-catalog/activities])

    (p.eql/process
     env
     {:list [{:course/id 4} {:course/id 1}]}
     [{:list [:course-category/id
              :course/id
              :course/new?
              {:course/activities [:course-activity/id
                                   :course-activity/image]}]}])

    (p.eql/process
     env
     {:list [{:course-category/id 2} {:course-category/id 1} {:course-category/id 3}]}
     [{:list [:course-category/id
              {:course-category/courses
               [:course/id
                :course/new?
                {:course/activities [:course-activity/id
                                     :course-activity/image]}]}]}])

  ;; Activity
    (p.eql/process
     env
     [:course/activities])

    (p.eql/process
     env
     {:list [{:course/id 2} {:course/id 1}]}
     [{:list [:course/activities :course-category/id]}])

   ;; Activity
    (p.eql/process
     env
     [{:list [:course-category/courses :course-category/id]}])

    (p.eql/process
     env
     {:list [{:course/id 2} {:course/id 1}]}
     [{:list [:course-category/courses :course-category/id]}])

    (p.eql/process
     env
     {:list [{:course/id 2} {:course/id 1}]}
     [{:list [:course-category/courses :course-category/id]}])
    |)
;; (get-courses* sql
  ;;                      {:columns courses-columns
  ;;                       ;:course/ids [1]
  ;;                       :course-category/ids [2]}))
;(store/get-courses-in-categories sql/sql [1 2])

(def query
  [{:course-categories
    [:course-category/id
     {:course-category/title-resource [:lang-resource/rid {:lang-resource/resources [:language/code :lang-resource/value]}]}
     {:course-category/courses
      [:course/id
       :course/key
       :course/title-rid
       {:course/title-resource [:lang-resource/rid {:lang-resource/resources [:language/code :lang-resource/value]}]}
       #_|]}]}])

(defn run [_opts]
  (println "query")
  (println "---------------")
  (clojure.pprint/pprint query)
  (println "result")
  (println "---------------")
  (clojure.pprint/pprint (p.eql/process env query))
  (System/exit 0))


;; (defn -main []
;;   (run {}))
;; (-main)
