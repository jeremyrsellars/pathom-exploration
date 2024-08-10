(ns pathom-resolvers.ex4
  (:require clojure.pprint
           [com.wsscode.pathom3.connect.operation :as pco]
           [com.wsscode.pathom3.connect.indexes :as pci]
           [com.wsscode.pathom3.interface.eql :as p.eql]
           [com.wsscode.pathom.viz.ws-connector.pathom3 :as p.connector]
           [com.wsscode.pathom.viz.ws-connector.core :as pvc]))

(def users [{"the-user" 1, "another-user" 2}])

(pco/defresolver klass-redirect-url
  [_ {:keys [:klass/code],
      teacher-id :user/id :as x}]
  {::pco/input [:klass/code
                :user/id]
   ::pco/output [:yessir.teacher.front.klass/klass-home-url]}
  {:yessir.teacher.front.klass/klass-home-url
   #p
   (str "/teacher/" teacher-id "/home#/teacher/main/classes/" code)})

(pco/defresolver user-id
  [_ {:keys [user/username]}]
  {:user/id
   (get users username)})

(pco/defresolver klasses
  [_ _]
  {::pco/input [:user/id]}
  {:klasses
   [{:klass/id 1, :klass/code "AAA111"}]})

(def resolvers [klass-redirect-url
                user-id
                klasses])

(p.eql/process
  (pci/register resolvers)
  {:user/username "the-user"
   #_#_:user/id       1}
  [{:klasses [:klass/id
              :klass/code
              :yessir.teacher.front.klass/klass-home-url]}])

; Pathom can't find a path for the following elements in the query: [:yessir.teacher.front.klass/klass-home-url] at path [:klasses 0]
