{:user #=(merge
          {:plugins [[lein-try "0.4.1"]
                     [lein-pprint "1.1.1"]
                     [lein-datomic-pro "0.0.1-SNAPSHOT"]]}
          #=(eval (try (read-string (slurp (str (System/getProperty "user.home")
                                                "/.config/datomic.clj")))
                       (catch Exception e {}))))}
