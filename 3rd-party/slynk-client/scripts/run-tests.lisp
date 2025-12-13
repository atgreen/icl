(require :asdf)

(asdf:clear-configuration)

(asdf:operate 'asdf:load-op ':slynk-client)
(asdf:operate 'asdf:load-op ':slynk-client/test)

;; Insert 5am project suite name
;; (5am:run! ...)

(in-package :slynk-client-test)

(5am:run! 'test-slynk-client)

(sb-ext::quit)
