(require :asdf)

(asdf:load-system :slynk-client)
(asdf:load-system :slynk-client/test)

(asdf:make :slynk-client)
(asdf:make :slynk-client)

(quit)
