(lang dune 1.2)
(name bimap)
(version 20190311)


;FAILS WITH: unknown field opam
;(opam
; (tags org:none)
; (depends
;   (ocaml (>= 4.06.0))
;   (core (>= v0.11.3))
;   (ocamlfind (>= 1.8))
;   (dune (>= 1.2.1))
;   (oUnit (>= 2.08))
; )
; (package
;  (name bimap)
;  (synopsis "An OCaml library implementing bi-directional maps and multi-maps")
;  (description "Bi-directional maps permit clients to not only obtain a value provided a key, but to obtain a key provided a value.")
;  (depends
;   (ocaml (>= 4.06.0))
;   (core (>= v0.11.3))
;   (ocamlfind (>= 1.8))
;   (dune (>= 1.2.1))
;   (oUnit (>= 2.08))
;  )
; )
;)