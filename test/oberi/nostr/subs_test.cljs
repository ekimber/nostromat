(ns oberi.nostr.subs-test
  (:require [cljs.test :as t :refer-macros [deftest is async]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [oberi.nostr.subs :as s]
            [cljs.pprint :refer [pprint]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(deftest replying-str
  (let [p-n {"a7f72cd8c8c7cf18fa6f44c131e01d5b88c2f47723a56626ef33d6990e6a9f15" "jools"
             "d307643547703537dfdef811c3dea96f1f9e84c8249e200353425924a9908cf8" "jops"
             "1c9a73fb9d999fc4ab930c9bf0cede1efe3d53d55f4398b6be8427cedcfa4d1f" "stoo"
             "1711344fd5fef76cb54f5fbfd77032c91d41a1ee032878d42f12572737633952" "bim"}
        ps '(("a7f72cd8c8c7cf18fa6f44c131e01d5b88c2f47723a56626ef33d6990e6a9f15")
             ("d307643547703537dfdef811c3dea96f1f9e84c8249e200353425924a9908cf8")
             ("1c9a73fb9d999fc4ab930c9bf0cede1efe3d53d55f4398b6be8427cedcfa4d1f")
             ("1711344fd5fef76cb54f5fbfd77032c91d41a1ee032878d42f12572737633952"))]
    (is (= '("jools" "jops" "stoo" "bim") (s/replying-str p-n ps)))
    (is (= '("jools" "jops" "stoo" "bim" "and 1 others") (s/replying-str p-n (conj ps "1c9a73fb9d999fc4ab930c9bf0cede1efe3d53d55f4398b6be8427cedcfa4d1f"))))
    (is (= '("jools" "jops" "stoo" "npub15…gz2796") (s/replying-str (dissoc p-n "1711344fd5fef76cb54f5fbfd77032c91d41a1ee032878d42f12572737633952") ps)))))
  
