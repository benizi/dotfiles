{:user #=(merge
          {:plugins [[lein-try "0.4.1"]
                     [lein-pprint "1.1.1"]
                     [cider/cider-nrepl "0.9.0-SNAPSHOT"]]}
          #=(eval (try (read-string (slurp (str (System/getProperty "user.home")
                                                "/.config/datomic.clj")))
                       (catch Exception e {}))))}
