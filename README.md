# Playground for exploring pathom3 usage patterns

## Example 1

[ex1.clj](/src/pathom_bug/ex1.clj)

### Usage

    clj -X pathom-bug.ex1/run

### Query, result, description of bug

query
---------------

```edn
[{:child-parents
  [:parent/id
   #:parent{:title-resource
            [:lang-resource/rid
             #:lang-resource{:resources [:language/code :lang-resource/value]}]} ; <-- these work
   #:parent{:children
            [:child/id
             :child/title-rid
             #:child{:title-resource                                   ; <-- does not work
                     [:lang-resource/rid                               ; <-- does not work
                      #:lang-resource{:resources                       ; <-- does not work
                                      [:language/code                  ; <-- does not work
                                       :lang-resource/value]}]}]}]}]   ; <-- does not work
```
result
---------------
```edn
{:child-parents
 [#:parent{:id 1,
           :title-resource
           #:lang-resource{:rid 1,
                           :resources
                           [{:language/code :language/en,
                             :lang-resource/value
                             {:language/id 1,
                              :language/code :language/en,
                              :lang-resource/rid 1,
                              :lang-resource/rid-code "en-1",
                              :lang-resource/value "en:1"}}
                            {:language/code :language/es,
                             :lang-resource/value
                             {:language/id 2,
                              :language/code :language/es,
                              :lang-resource/rid 1,
                              :lang-resource/rid-code "es-1",
                              :lang-resource/value "es:1"}}]},
           :children
           [#:child{:id 101, :title-rid 1}                             ; <-- missing :child/title-resource
            #:child{:id 102,
                    :title-rid 3,
                    :title-resource #:lang-resource{:resources
                                                    {}                 ; <-- should be a vector w/ 2 entries
                                                    }}]}]}
```
