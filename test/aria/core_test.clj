(ns aria.core-test
  (:require [clojure.test :refer [deftest is]]
            [aria.core :as aria]))

;; Test documents are hand-built plain maps in the shape aria.core expects:
;; {:root <id> :focus <id-or-nil> :nodes {<id> {:node/id <id> :node/type ... ...}}}
;; Real producers (kotoba.wasm.dom / browser.dom-bridge) always stamp each
;; node's own key back onto it as :node/id -- aria.core's fieldset/label
;; ancestor-walking logic depends on that, so these helpers do the same.

(defn- el
  ([id tag] (el id tag {} []))
  ([id tag attrs] (el id tag attrs []))
  ([id tag attrs children]
   {:node/id id
    :node/type :element
    :tag tag
    :attrs attrs
    :children (vec children)}))

(defn- txt
  [id text]
  {:node/id id :node/type :text :text text})

(defn- doc
  ([nodes root] (doc nodes root nil))
  ([nodes root focus]
   {:root root
    :focus focus
    :nodes (into {} (map (juxt :node/id identity)) nodes)}))

(defn- find-by-id
  [tree id]
  (or (when (= id (:a11y/id tree))
        tree)
      (some #(find-by-id % id) (:a11y/children tree))))

(deftest implicit-roles-cover-common-tags-and-headings
  (let [d (doc [(el :page :main {} [:link :anchor :para :quote :pre-el :code-el
                                     :strong-el :em-el :dl-el :figure-el :ul-el
                                     :header-el :footer-el :nav-el :section-el
                                     :article-el :aside-el :form-el
                                     :h1-el :h2-el :h3-el])
                (el :link :a {:href "/docs"} [:link-txt])
                (txt :link-txt "Docs")
                (el :anchor :a {} [:anchor-txt])
                (txt :anchor-txt "Anchor")
                (el :para :p {} [:para-txt])
                (txt :para-txt "Body")
                (el :quote :blockquote {} [:quote-txt])
                (txt :quote-txt "Quote")
                (el :pre-el :pre {} [:pre-txt])
                (txt :pre-txt "code")
                (el :code-el :code {} [:code-txt])
                (txt :code-txt "x")
                (el :strong-el :strong {} [:strong-txt])
                (txt :strong-txt "Strong")
                (el :em-el :em {} [:em-txt])
                (txt :em-txt "Em")
                (el :dl-el :dl {} [:dt-el :dd-el])
                (el :dt-el :dt {} [:dt-txt])
                (txt :dt-txt "Name")
                (el :dd-el :dd {} [:dd-txt])
                (txt :dd-txt "Kotoba")
                (el :figure-el :figure {} [:figcaption-el])
                (el :figcaption-el :figcaption {} [:figcaption-txt])
                (txt :figcaption-txt "Caption")
                (el :ul-el :ul {} [:li-el])
                (el :li-el :li {} [:li-txt])
                (txt :li-txt "One")
                (el :header-el :header)
                (el :footer-el :footer)
                (el :nav-el :nav)
                (el :section-el :section)
                (el :article-el :article)
                (el :aside-el :aside)
                (el :form-el :form)
                (el :h1-el :h1 {} [:h1-txt])
                (txt :h1-txt "One")
                (el :h2-el :h2 {} [:h2-txt])
                (txt :h2-txt "Two")
                (el :h3-el :h3 {} [:h3-txt])
                (txt :h3-txt "Three")]
               :page)
        tree (aria/tree d)]
    (is (= "main" (:a11y/role tree)))
    (is (= "link" (:a11y/role (find-by-id tree :link))))
    (is (= "generic" (:a11y/role (find-by-id tree :anchor))))
    (is (= "paragraph" (:a11y/role (find-by-id tree :para))))
    (is (= "blockquote" (:a11y/role (find-by-id tree :quote))))
    (is (= "code" (:a11y/role (find-by-id tree :pre-el))))
    (is (= "code" (:a11y/role (find-by-id tree :code-el))))
    (is (= "strong" (:a11y/role (find-by-id tree :strong-el))))
    (is (= "emphasis" (:a11y/role (find-by-id tree :em-el))))
    (is (= "list" (:a11y/role (find-by-id tree :dl-el))))
    (is (= "term" (:a11y/role (find-by-id tree :dt-el))))
    (is (= "definition" (:a11y/role (find-by-id tree :dd-el))))
    (is (= "figure" (:a11y/role (find-by-id tree :figure-el))))
    (is (= "caption" (:a11y/role (find-by-id tree :figcaption-el))))
    (is (= "list" (:a11y/role (find-by-id tree :ul-el))))
    (is (= "listitem" (:a11y/role (find-by-id tree :li-el))))
    (is (= "banner" (:a11y/role (find-by-id tree :header-el))))
    (is (= "contentinfo" (:a11y/role (find-by-id tree :footer-el))))
    (is (= "navigation" (:a11y/role (find-by-id tree :nav-el))))
    (is (= "region" (:a11y/role (find-by-id tree :section-el))))
    (is (= "article" (:a11y/role (find-by-id tree :article-el))))
    (is (= "complementary" (:a11y/role (find-by-id tree :aside-el))))
    (is (= "form" (:a11y/role (find-by-id tree :form-el))))
    (is (= "heading" (:a11y/role (find-by-id tree :h1-el))))
    (is (= 1 (:a11y/level (find-by-id tree :h1-el))))
    (is (= 2 (:a11y/level (find-by-id tree :h2-el))))
    (is (= 3 (:a11y/level (find-by-id tree :h3-el))))))

;; ---- implicit aria-level for <h3>-<h6> -- previously only :h1/:h2 were
;; handled by the case in accessible-node, so h3-h6 (which DO map to role
;; "heading" via implicit-roles) fell through to nil instead of the
;; HTML-AAM-mandated 3-6, breaking level-based heading navigation (a
;; common real screen-reader feature) for the 4 most commonly nested
;; heading tags. Confirmed via direct REPL reproduction before touching
;; source. ----

(deftest implicit-heading-level-covers-h3-through-h6
  (let [d (doc [(el :page :main {} [:h3-el :h4-el :h5-el :h6-el])
                (el :h3-el :h3 {} [:h3-txt]) (txt :h3-txt "Three")
                (el :h4-el :h4 {} [:h4-txt]) (txt :h4-txt "Four")
                (el :h5-el :h5 {} [:h5-txt]) (txt :h5-txt "Five")
                (el :h6-el :h6 {} [:h6-txt]) (txt :h6-txt "Six")]
               :page)
        tree (aria/tree d)]
    (is (= 3 (:a11y/level (find-by-id tree :h3-el))))
    (is (= 4 (:a11y/level (find-by-id tree :h4-el))))
    (is (= 5 (:a11y/level (find-by-id tree :h5-el))))
    (is (= 6 (:a11y/level (find-by-id tree :h6-el))))))

(deftest explicit-aria-level-still-overrides-the-implicit-heading-level
  (let [d (doc [(el :page :main {} [:h3-el])
                (el :h3-el :h3 {:aria-level "9"} [:h3-txt])
                (txt :h3-txt "Three")]
               :page)
        tree (aria/tree d)]
    (is (= "9" (:a11y/level (find-by-id tree :h3-el))))))

(deftest table-structure-and-form-element-implicit-roles
  (let [d (doc [(el :page :main {} [:table-el :select-el :textarea-el])
                (el :table-el :table {} [:thead-el])
                (el :thead-el :thead {} [:row-el])
                (el :row-el :tr {} [:th-col :th-row :td-el])
                (el :th-col :th {} [:th-col-txt])
                (txt :th-col-txt "Name")
                (el :th-row :th {:scope "row"} [:th-row-txt])
                (txt :th-row-txt "Project")
                (el :td-el :td {} [:td-txt])
                (txt :td-txt "Kotoba")
                (el :select-el :select)
                (el :textarea-el :textarea)]
               :page)
        tree (aria/tree d)]
    (is (= "table" (:a11y/role (find-by-id tree :table-el))))
    (is (= "rowgroup" (:a11y/role (find-by-id tree :thead-el))))
    (is (= "row" (:a11y/role (find-by-id tree :row-el))))
    (is (= "columnheader" (:a11y/role (find-by-id tree :th-col))))
    (is (= "rowheader" (:a11y/role (find-by-id tree :th-row))))
    (is (= "cell" (:a11y/role (find-by-id tree :td-el))))
    (is (= "combobox" (:a11y/role (find-by-id tree :select-el))))
    (is (= "textbox" (:a11y/role (find-by-id tree :textarea-el))))))

;; ---- <progress>/<meter>/<dialog>/<hr>/<output> implicit roles -- none of
;; these five were in implicit-roles at all, so each fell through to the
;; generic default "generic". Per HTML-AAM's implicit-ARIA-semantics
;; table: progress -> progressbar, meter -> meter, dialog -> dialog,
;; hr -> separator, output -> status. progressbar/dialog in particular
;; are two very common native widgets -- a real, materially broken
;; accessibility gap, not cosmetic (a screen reader has no idea a real
;; <dialog open> just became the modal focus, or that a <progress> is
;; even a progress indicator at all). Confirmed via direct REPL
;; reproduction before touching source. ----

(deftest progress-meter-dialog-hr-output-have-their-own-implicit-roles
  (let [d (doc [(el :page :main {} [:progress-el :meter-el :dialog-el :hr-el :output-el])
                (el :progress-el :progress {:value "30" :max "100"})
                (el :meter-el :meter {:value "3" :min "0" :max "10"})
                (el :dialog-el :dialog {:open "open"})
                (el :hr-el :hr)
                (el :output-el :output)]
               :page)
        tree (aria/tree d)]
    (is (= "progressbar" (:a11y/role (find-by-id tree :progress-el))))
    (is (= "meter" (:a11y/role (find-by-id tree :meter-el))))
    (is (= "dialog" (:a11y/role (find-by-id tree :dialog-el))))
    (is (= "separator" (:a11y/role (find-by-id tree :hr-el))))
    (is (= "status" (:a11y/role (find-by-id tree :output-el))))))

(deftest explicit-role-still-overrides-the-new-implicit-role-mappings
  (let [d (doc [(el :page :main {} [:progress-el])
                (el :progress-el :progress {:role "img"})]
               :page)
        tree (aria/tree d)]
    (is (= "img" (:a11y/role (find-by-id tree :progress-el))))))

;; ---- <option>/<optgroup> implicit roles, and <select size=N> (N>1, no
;; `multiple`) mapping to "listbox" per HTML-AAM -- previously <option>/
;; <optgroup> weren't in implicit-roles at all (fell through to "generic"),
;; and only `multiple` was ever checked for the listbox override, so a
;; common native multi-row picker (<select size="4">, no multiple needed)
;; was exposed as a "combobox" whose visible rows all reported role
;; "generic" -- a materially broken accessibility tree, not cosmetic.
;; Confirmed via direct REPL reproduction before touching source. ----

(deftest option-and-optgroup-have-their-own-implicit-roles
  (let [d (doc [(el :page :main {} [:select-el])
                (el :select-el :select {} [:group-el])
                (el :group-el :optgroup {} [:opt-el])
                (el :opt-el :option {:value "a"})]
               :page)
        tree (aria/tree d)]
    (is (= "group" (:a11y/role (find-by-id tree :group-el))))
    (is (= "option" (:a11y/role (find-by-id tree :opt-el))))))

(deftest select-size-greater-than-one-maps-to-listbox-without-multiple
  (let [d (doc [(el :page :main {} [:select-el])
                (el :select-el :select {:size "4"} [:opt-el])
                (el :opt-el :option {:value "a" :selected "selected"})]
               :page)
        tree (aria/tree d)
        select (find-by-id tree :select-el)]
    (is (= "listbox" (:a11y/role select)))
    (is (= ["a"] (:a11y/values select)))))

(deftest select-size-of-one-or-absent-stays-combobox
  (let [d (doc [(el :page :main {} [:size-one :size-absent])
                (el :size-one :select {:size "1"} [:o1])
                (el :o1 :option {:value "a"})
                (el :size-absent :select {} [:o2])
                (el :o2 :option {:value "b"})]
               :page)
        tree (aria/tree d)]
    (is (= "combobox" (:a11y/role (find-by-id tree :size-one))))
    (is (= "combobox" (:a11y/role (find-by-id tree :size-absent))))))

(deftest input-type-derived-roles-and-values
  (let [d (doc [(el :page :main {} [:checkbox-input :radio-input :submit-input
                                     :range-input :search-input :number-input
                                     :text-input :password-input])
                (el :checkbox-input :input {:type "checkbox"})
                (el :radio-input :input {:type "radio"})
                (el :submit-input :input {:type "submit" :value "Go"})
                (el :range-input :input {:type "range" :value "4"})
                (el :search-input :input {:type "search" :value "query"})
                (el :number-input :input {:type "number" :value "7"})
                (el :text-input :input {:value "a@example.test"})
                (el :password-input :input {:type "password" :value "secret"})]
               :page)
        tree (aria/tree d)]
    (is (= "checkbox" (:a11y/role (find-by-id tree :checkbox-input))))
    (is (= "radio" (:a11y/role (find-by-id tree :radio-input))))
    (is (= "button" (:a11y/role (find-by-id tree :submit-input))))
    (is (= "Go" (:a11y/value (find-by-id tree :submit-input))))
    (is (= "slider" (:a11y/role (find-by-id tree :range-input))))
    (is (= "4" (:a11y/value (find-by-id tree :range-input))))
    (is (= "searchbox" (:a11y/role (find-by-id tree :search-input))))
    (is (= "query" (:a11y/value (find-by-id tree :search-input))))
    (is (= "spinbutton" (:a11y/role (find-by-id tree :number-input))))
    (is (= "7" (:a11y/value (find-by-id tree :number-input))))
    (is (= "textbox" (:a11y/role (find-by-id tree :text-input))))
    (is (= "a@example.test" (:a11y/value (find-by-id tree :text-input))))
    (is (= "textbox" (:a11y/role (find-by-id tree :password-input))))
    (is (= "******" (:a11y/value (find-by-id tree :password-input))))))

(deftest hidden-nodes-are-skipped-from-the-tree
  (let [d (doc [(el :page :main {} [:heading :aria-hidden-section :presentation-section
                                     :css-hidden-section :hidden-type-input :attr-hidden-el
                                     :visible-button])
                (el :heading :h1 {} [:heading-txt])
                (txt :heading-txt "Hello")
                (el :aria-hidden-section :section {:aria-hidden "true"} [:hidden-button])
                (el :hidden-button :button {} [:hidden-button-txt])
                (txt :hidden-button-txt "Hidden")
                (el :presentation-section :section {:role "presentation"} [:also-hidden-button])
                (el :also-hidden-button :button {} [:also-hidden-txt])
                (txt :also-hidden-txt "Also hidden")
                (el :css-hidden-section :section {:style/display "none"} [:css-hidden-button])
                (el :css-hidden-button :button {} [:css-hidden-txt])
                (txt :css-hidden-txt "CSS hidden")
                (el :hidden-type-input :input {:type "hidden" :value "token"})
                (el :attr-hidden-el :span {:hidden true} [:attr-hidden-txt])
                (txt :attr-hidden-txt "Attr hidden")
                (el :visible-button :button {} [:visible-txt])
                (txt :visible-txt "Visible")]
               :page)
        tree (aria/tree d)]
    (is (= ["heading" "button"]
           (mapv :a11y/role (:a11y/children tree))))
    (is (= "HelloVisible" (:a11y/name tree)))
    (is (= "Hello" (:a11y/name (find-by-id tree :heading))))
    (is (nil? (find-by-id tree :aria-hidden-section)))
    (is (nil? (find-by-id tree :hidden-button)))
    (is (nil? (find-by-id tree :presentation-section)))
    (is (nil? (find-by-id tree :also-hidden-button)))
    (is (nil? (find-by-id tree :css-hidden-section)))
    (is (nil? (find-by-id tree :css-hidden-button)))
    (is (nil? (find-by-id tree :hidden-type-input)))
    (is (nil? (find-by-id tree :attr-hidden-el)))
    (is (= "Visible" (:a11y/name (find-by-id tree :visible-button))))))

(deftest bare-hidden-attribute-value-of-empty-string-is-also-skipped
  ;; Real bug this guards: hidden? previously did a raw (= true ...)
  ;; check instead of this file's own truthy-attr? helper -- this test
  ;; document uses "" for :hidden, the REAL value a spec-correct HTML
  ;; parser produces for a bare `<span hidden>` (Element.getAttribute()
  ;; must return "" for a bare boolean attribute, not the string
  ;; "true"), unlike the sibling test above which used the Clojure
  ;; boolean `true` directly and so never exercised this gap. Confirmed
  ;; via a real downstream test failure (kotoba-lang/browser's own
  ;; accessibility-tree-skips-hidden-and-presentation-nodes) before
  ;; fixing.
  (let [d (doc [(el :page :main {} [:visible-span :hidden-span])
                (el :visible-span :span {} [:visible-txt])
                (txt :visible-txt "Visible")
                (el :hidden-span :span {:hidden ""} [:hidden-txt])
                (txt :hidden-txt "Hidden")]
               :page)
        tree (aria/tree d)]
    (is (nil? (find-by-id tree :hidden-span)))
    (is (= "Visible" (:a11y/name tree)))))

;; ---- <img alt=""> is a decorative image -- excluded from the tree ----
;;
;; Real bug this guards: implicit-roles unconditionally mapped :img -> "img"
;; with no check of the `alt` attribute's value at all, so a decorative
;; image (alt="", the standard idiom for a spacer/divider graphic) was
;; exposed with role "img" exactly like a real, meaningful image -- a
;; screen-reader user would hear an empty/meaningless image announced,
;; which every real browser actively suppresses. Confirmed via direct REPL
;; reproduction before touching source. Per HTML-AAM's own <img> role-
;; mapping table, alt="" computes an implicit role of presentation and
;; must be excluded from the tree, same as an explicit
;; role="presentation" node already is.

(deftest decorative-image-with-empty-alt-is-excluded-from-the-tree
  (let [d (doc [(el :page :main {} [:deco :real])
                (el :deco :img {:alt "" :src "spacer.png"})
                (el :real :img {:alt "A real photo" :src "photo.png"})]
               :page)
        tree (aria/tree d)]
    (is (nil? (find-by-id tree :deco))
        "alt=\"\" must compute implicit role presentation and be dropped, not exposed as role img")
    (is (= "img" (:a11y/role (find-by-id tree :real))))
    (is (= "A real photo" (:a11y/name (find-by-id tree :real))))))

(deftest image-with-no-alt-attribute-at-all-still-gets-the-ordinary-img-role
  ;; A MISSING alt attribute is a separate, unrelated accessibility gap
  ;; (an authoring mistake to flag, not a decorative image) -- it must
  ;; NOT be treated the same as alt="", or every unlabeled <img> would
  ;; silently vanish from the tree instead of surfacing as a real problem.
  (let [d (doc [(el :page :main {} [:no-alt])
                (el :no-alt :img {:src "mystery.png"})]
               :page)
        tree (aria/tree d)]
    (is (some? (find-by-id tree :no-alt)))
    (is (= "img" (:a11y/role (find-by-id tree :no-alt))))))

(deftest explicit-role-overrides-the-implicit-decorative-image-default
  ;; An author explicitly overriding role="img" on an alt="" image (an
  ;; ARIA-precedence edge case -- unusual but valid) must win over the
  ;; implicit presentation default, matching this file's own convention
  ;; everywhere else that an explicit role attribute always wins.
  (let [d (doc [(el :page :main {} [:override])
                (el :override :img {:alt "" :role "img" :src "overridden.png"})]
               :page)
        tree (aria/tree d)]
    (is (some? (find-by-id tree :override)))
    (is (= "img" (:a11y/role (find-by-id tree :override))))))

(deftest form-controls-project-accessibility-state
  (let [d (doc [(el :page :main {} [:field :flag :run :mode :tags])
                (el :field :input {:placeholder "Name" :value "Kotoba"
                                    :selection-start "1" :selection-end "4"
                                    :composition "to" :readonly true})
                (el :flag :input {:type "checkbox" :checked true :disabled true
                                   :aria-label "Accept"})
                (el :run :button {:disabled true} [:run-txt])
                (txt :run-txt "Run")
                (el :mode :select {} [:mode-one :mode-two])
                (el :mode-one :option {:value "one" :selected false} [:mode-one-txt])
                (txt :mode-one-txt "One")
                (el :mode-two :option {:value "two" :selected true} [:mode-two-txt])
                (txt :mode-two-txt "Two")
                (el :tags :select {:multiple true}
                    [:tag-one :tag-open-group :tag-locked-group :tag-three])
                (el :tag-one :option {:value "one" :selected true} [:tag-one-txt])
                (txt :tag-one-txt "One")
                (el :tag-open-group :optgroup {:label "Open"} [:tag-two])
                (el :tag-two :option {:value "two" :selected true} [:tag-two-txt])
                (txt :tag-two-txt "Two")
                (el :tag-locked-group :optgroup {:label "Locked" :disabled true} [:tag-locked])
                (el :tag-locked :option {:value "locked" :selected true} [:tag-locked-txt])
                (txt :tag-locked-txt "Locked")
                (el :tag-three :option {:value "three" :selected false} [:tag-three-txt])
                (txt :tag-three-txt "Three")]
               :page)
        tree (aria/tree d)
        field (find-by-id tree :field)
        flag (find-by-id tree :flag)
        run (find-by-id tree :run)
        mode (find-by-id tree :mode)
        tags (find-by-id tree :tags)
        tag-one (find-by-id tree :tag-one)
        tag-two (find-by-id tree :tag-two)
        tag-locked (find-by-id tree :tag-locked)
        tag-three (find-by-id tree :tag-three)]
    (is (= "textbox" (:a11y/role field)))
    (is (= "Name" (:a11y/name field)))
    (is (= "Kotoba" (:a11y/value field)))
    (is (= 1 (:a11y/selection-start field))
        "now a real, clamped integer offset (not the raw attribute string) -- see the stale-selection tests below")
    (is (= 4 (:a11y/selection-end field)))
    (is (= "to" (:a11y/composition field)))
    (is (= true (:a11y/readonly field)))
    (is (= "checkbox" (:a11y/role flag)))
    (is (= "Accept" (:a11y/name flag)))
    (is (= true (:a11y/checked flag)))
    (is (= true (:a11y/disabled flag)))
    (is (= "button" (:a11y/role run)))
    (is (= "Run" (:a11y/name run)))
    (is (= true (:a11y/disabled run)))
    (is (= "combobox" (:a11y/role mode)))
    (is (= "two" (:a11y/value mode)))
    (is (= "listbox" (:a11y/role tags)))
    (is (= "one two" (:a11y/value tags)))
    (is (= ["one" "two"] (:a11y/values tags)))
    (is (= true (:a11y/selected tag-one)))
    (is (= true (:a11y/selected tag-two)))
    (is (= true (:a11y/selected tag-locked)))
    (is (= false (:a11y/selected tag-three)))))

(deftest disabled-fieldset-projects-accessibility-state
  (let [d (doc [(el :page :main {} [:fieldset-el])
                (el :fieldset-el :fieldset {:disabled true}
                    [:legend-el :field :run :mode])
                (el :legend-el :legend {} [:legend-field :legend-button])
                (el :legend-field :input {:value "enabled"})
                (el :legend-button :button {} [:legend-button-txt])
                (txt :legend-button-txt "Legend")
                (el :field :input {:value "blocked"})
                (el :run :button {} [:run-txt])
                (txt :run-txt "Run")
                (el :mode :select {} [:mode-one])
                (el :mode-one :option {:value "one"})]
               :page)
        tree (aria/tree d)]
    (is (= false (:a11y/disabled (find-by-id tree :legend-field))))
    (is (= false (:a11y/disabled (find-by-id tree :legend-button))))
    (is (= true (:a11y/disabled (find-by-id tree :field))))
    (is (= true (:a11y/disabled (find-by-id tree :run))))
    (is (= true (:a11y/disabled (find-by-id tree :mode))))))

(deftest aria-state-projects-accessibility-state
  (let [d (doc [(el :page :main {} [:toggle :hint :panel :check :custom :step-link :option-el])
                (el :toggle :button {:aria-pressed "mixed" :aria-expanded "true"
                                      :aria-controls "panel-dom missing"
                                      :aria-describedby "hint-dom"} [:toggle-txt])
                (txt :toggle-txt "Toggle")
                (el :hint :p {:id "hint-dom"} [:hint-txt])
                (txt :hint-txt "More detail")
                (el :panel :section {:id "panel-dom"} [:panel-txt])
                (txt :panel-txt "Panel")
                (el :check :div {:role "checkbox" :aria-checked "mixed"} [:check-txt])
                (txt :check-txt "Choice")
                (el :custom :div {:role "textbox" :aria-disabled "true" :aria-required "true"
                                   :aria-readonly "true" :aria-invalid "spelling"} [:custom-txt])
                (txt :custom-txt "Custom")
                (el :step-link :a {:aria-current "step"} [:step-txt])
                (txt :step-txt "Step")
                (el :option-el :option {:aria-selected "true"} [:option-txt])
                (txt :option-txt "Mode")]
               :page)
        tree (aria/tree d)
        toggle (find-by-id tree :toggle)
        panel (find-by-id tree :panel)
        check (find-by-id tree :check)
        custom (find-by-id tree :custom)
        step (find-by-id tree :step-link)
        option (find-by-id tree :option-el)]
    (is (= "mixed" (:a11y/pressed toggle)))
    (is (= true (:a11y/expanded toggle)))
    (is (= [(:a11y/id panel)] (:a11y/controls toggle)))
    (is (= "More detail" (:a11y/description toggle)))
    (is (= "mixed" (:a11y/checked check)))
    (is (= true (:a11y/disabled custom)))
    (is (= true (:a11y/required custom)))
    (is (= true (:a11y/readonly custom)))
    (is (= "spelling" (:a11y/invalid custom)))
    (is (= "step" (:a11y/current step)))
    (is (= true (:a11y/selected option)))))

(deftest aria-structure-and-range-project-accessibility-state
  (let [d (doc [(el :page :main {} [:heading :item :grid :slider])
                (el :heading :div {:role "heading" :aria-level "3"} [:heading-txt])
                (txt :heading-txt "Section")
                (el :item :div {:role "listitem" :aria-posinset "2" :aria-setsize "5"} [:item-txt])
                (txt :item-txt "Item")
                (el :grid :div {:role "grid" :aria-rowcount "10" :aria-colcount "4"} [:cell])
                (el :cell :div {:role "gridcell" :aria-rowindex "3" :aria-colindex "2"} [:cell-txt])
                (txt :cell-txt "Cell")
                (el :slider :div {:role "slider" :aria-valuemin "0" :aria-valuemax "100"
                                   :aria-valuenow "40" :aria-valuetext "Forty"
                                   :aria-orientation "vertical"} [:slider-txt])
                (txt :slider-txt "Volume")]
               :page)
        tree (aria/tree d)]
    (is (= "3" (:a11y/level (find-by-id tree :heading))))
    (is (= "2" (:a11y/posinset (find-by-id tree :item))))
    (is (= "5" (:a11y/setsize (find-by-id tree :item))))
    (is (= "10" (:a11y/rowcount (find-by-id tree :grid))))
    (is (= "4" (:a11y/colcount (find-by-id tree :grid))))
    (is (= "3" (:a11y/rowindex (find-by-id tree :cell))))
    (is (= "2" (:a11y/colindex (find-by-id tree :cell))))
    (is (= "0" (:a11y/valuemin (find-by-id tree :slider))))
    (is (= "100" (:a11y/valuemax (find-by-id tree :slider))))
    (is (= "40" (:a11y/valuenow (find-by-id tree :slider))))
    (is (= "Forty" (:a11y/valuetext (find-by-id tree :slider))))
    (is (= "vertical" (:a11y/orientation (find-by-id tree :slider))))))

(deftest aria-live-region-projects-accessibility-state
  (let [d (doc [(el :page :main {} [:status])
                (el :status :section {:role "status" :aria-live "polite" :aria-busy "true"
                                       :aria-atomic "false" :aria-relevant "additions text"}
                    [:status-txt])
                (txt :status-txt "Loading")]
               :page)
        tree (aria/tree d)
        status (find-by-id tree :status)]
    (is (= "polite" (:a11y/live status)))
    (is (= true (:a11y/busy status)))
    (is (= false (:a11y/atomic status)))
    (is (= "additions text" (:a11y/relevant status)))))

(deftest required-and-invalid-state-with-validation-reason
  (let [d (doc [(el :page :main {} [:field :code :valid-field])
                (el :field :input {:required true :invalid true
                                    :validation-reason "value-missing"})
                (el :code :input {:value "ab" :invalid true :validation-reason "too-short"})
                (el :valid-field :input {:required true :value "A"})]
               :page)
        tree (aria/tree d)]
    (is (= true (:a11y/required (find-by-id tree :field))))
    (is (= true (:a11y/invalid (find-by-id tree :field))))
    (is (= "value-missing" (:a11y/validation-reason (find-by-id tree :field))))
    (is (= true (:a11y/invalid (find-by-id tree :code))))
    (is (= "too-short" (:a11y/validation-reason (find-by-id tree :code))))
    (is (= true (:a11y/required (find-by-id tree :valid-field))))
    (is (= false (:a11y/invalid (find-by-id tree :valid-field))))
    (is (nil? (:a11y/validation-reason (find-by-id tree :valid-field))))))

(deftest accessible-names-from-labels-alt-title-and-references
  (let [d (doc [(el :page :main {} [:label-for-el :labeled-input
                                     :wrap-label
                                     :visible-label-span :hidden-label-span
                                     :labelledby-input
                                     :logo-img :title-button])
                (el :label-for-el :label {:for "named"} [:label-for-txt])
                (txt :label-for-txt "First name")
                (el :labeled-input :input {:id "named" :value "Kotoba"})
                (el :wrap-label :label {} [:wrap-txt :wrapped-checkbox])
                (txt :wrap-txt "Accept ")
                (el :wrapped-checkbox :input {:id "flag" :type "checkbox" :checked true})
                (el :visible-label-span :span {:id "visible-label"} [:visible-label-txt])
                (txt :visible-label-txt "Visible label")
                (el :hidden-label-span :span {:id "hidden-label" :hidden true} [:hidden-label-txt])
                (txt :hidden-label-txt "Hidden label")
                (el :labelledby-input :input {:id "labelledby-input"
                                               :aria-labelledby "visible-label hidden-label"
                                               :placeholder "Ignored"})
                (el :logo-img :img {:alt "Kotoba mark"})
                (el :title-button :button {:id "title-button" :title "Settings"})]
               :page)
        tree (aria/tree d)]
    (is (= "textbox" (:a11y/role (find-by-id tree :labeled-input))))
    (is (= "First name" (:a11y/name (find-by-id tree :labeled-input))))
    (is (= "checkbox" (:a11y/role (find-by-id tree :wrapped-checkbox))))
    (is (= "Accept" (:a11y/name (find-by-id tree :wrapped-checkbox))))
    (is (= true (:a11y/checked (find-by-id tree :wrapped-checkbox))))
    (is (= "Visible label" (:a11y/name (find-by-id tree :labelledby-input))))
    (is (= "img" (:a11y/role (find-by-id tree :logo-img))))
    (is (= "Kotoba mark" (:a11y/name (find-by-id tree :logo-img))))
    (is (= "Settings" (:a11y/name (find-by-id tree :title-button))))))

;; ---- WAI-ARIA Accessible Name and Description Computation: aria-
;; labelledby (step 2A) must win over aria-label (step 2C) when an
;; element carries both -- name-for previously checked aria-label FIRST,
;; backwards from the spec. Confirmed via direct REPL reproduction
;; before touching source. ----

(deftest aria-labelledby-wins-over-aria-label-when-both-are-present
  (let [d (doc [(el :page :main {} [:btn :lbl])
                (el :btn :button {:aria-label "Wrong" :aria-labelledby "lbl"})
                (el :lbl :span {:id "lbl"} [:lbl-txt])
                (txt :lbl-txt "Correct")]
               :page)
        tree (aria/tree d)]
    (is (= "Correct" (:a11y/name (find-by-id tree :btn))))))

(deftest aria-label-still-used-when-aria-labelledby-is-absent
  (let [d (doc [(el :page :main {} [:btn])
                (el :btn :button {:aria-label "OnlyLabel"})]
               :page)
        tree (aria/tree d)]
    (is (= "OnlyLabel" (:a11y/name (find-by-id tree :btn))))))

(deftest aria-label-is-the-fallback-when-aria-labelledby-references-a-missing-id
  (let [d (doc [(el :page :main {} [:btn])
                (el :btn :button {:aria-label "Fallback" :aria-labelledby "missing"})]
               :page)
        tree (aria/tree d)]
    (is (= "Fallback" (:a11y/name (find-by-id tree :btn))))))

(deftest focused-node-projects-a11y-focused
  (let [d (doc [(el :page :main {} [:first-input :second-input])
                (el :first-input :input {:value "a"})
                (el :second-input :input {:value "b"})]
               :page
               :second-input)
        tree (aria/tree d)]
    (is (nil? (:a11y/focused (find-by-id tree :first-input))))
    (is (= true (:a11y/focused (find-by-id tree :second-input))))))

(deftest file-input-redacts-to-filename-only
  (let [d (doc [(el :page :main {} [:upload])
                (el :upload :input {:type "file"
                                     :files [{:file/name "report.csv"
                                              :file/type "text/csv"
                                              :file/size 42
                                              :file/path "/Users/me/report.csv"}]})]
               :page)
        tree (aria/tree d)
        upload (find-by-id tree :upload)]
    (is (= "textbox" (:a11y/role upload)))
    (is (= "report.csv" (:a11y/value upload)))
    (is (not= "/Users/me/report.csv" (:a11y/value upload)))))

(deftest scroll-container-projects-overflow-and-scroll-state
  (let [d (doc [(el :page :main {} [:pane])
                (el :pane :section {:overflow "auto" :scroll-top "24" :scroll-left "3"}
                    [:pane-txt])
                (txt :pane-txt "Scrolled")]
               :page)
        tree (aria/tree d)
        pane (find-by-id tree :pane)]
    (is (= "region" (:a11y/role pane)))
    (is (= "auto" (:a11y/overflow pane)))
    (is (= "24" (:a11y/scroll-top pane)))
    (is (= "3" (:a11y/scroll-left pane)))))

(deftest tree-unwraps-synthetic-document-wrapper
  (let [d (doc [(el :doc-root :document {} [:main-el])
                (el :main-el :main {} [:main-txt])
                (txt :main-txt "Hello")]
               :doc-root)
        tree (aria/tree d)]
    (is (= "main" (:a11y/role tree)))
    (is (= "Hello" (:a11y/name tree)))
    (is (= :main-el (:a11y/id tree)))))

(deftest accessible-node-computes-subtree-directly
  (let [d (doc [(el :page :main {} [:child])
                (el :child :button {} [:child-txt])
                (txt :child-txt "Run")]
               :page)]
    (is (= "button" (:a11y/role (aria/accessible-node d :child))))
    (is (= "Run" (:a11y/name (aria/accessible-node d :child))))))

;; ---- :a11y/selection-start/:a11y/selection-end must clamp to the real
;; value's own length, never a stale out-of-range offset ----
;;
;; Real bug this guards: previously copied straight from the node's own
;; attrs with zero validation against the real value's length -- the
;; exact same root-cause bug cssom.layout's own sel-ops already had to
;; guard against. The JS-facing value setter (browser.compat.quickjs-
;; wasm) has no spec-mandated selection-reset, so a real, ordinary
;; script sequence (set a longer value, select a range, then set a
;; SHORTER value) leaves stale offsets exceeding the new value's own
;; length -- exposed on the public accessibility-tree API assistive
;; tech and browser-use both read for form-field semantics. Confirmed
;; via direct REPL reproduction before touching source.

(deftest accessible-node-clamps-a-stale-selection-exceeding-the-shortened-values-length
  (let [d (doc [(el :field :input {:value "Ko" :selection-start "1" :selection-end "4"})]
               :field)
        field (aria/accessible-node d :field)]
    (is (= "Ko" (:a11y/value field)))
    (is (= 1 (:a11y/selection-start field))
        "still in range, unaffected")
    (is (= 2 (:a11y/selection-end field))
        "clamped down to the value's own 2-character length, not the stale 4")))

(deftest accessible-node-clamps-a-negative-selection-start-to-zero
  (let [d (doc [(el :field :input {:value "hi" :selection-start "-5" :selection-end "1"})]
               :field)
        field (aria/accessible-node d :field)]
    (is (= 0 (:a11y/selection-start field)))
    (is (= 1 (:a11y/selection-end field)))))

(deftest accessible-node-clamps-both-offsets-to-zero-on-an-empty-value
  (let [d (doc [(el :field :input {:value "" :selection-start "3" :selection-end "5"})]
               :field)
        field (aria/accessible-node d :field)]
    (is (= "" (:a11y/value field)))
    (is (= 0 (:a11y/selection-start field)))
    (is (= 0 (:a11y/selection-end field)))))
