
(let
  ((+         (ffi (fun (a a)    a )))
   (print-int (ffi (fun (Int)    Unit)))
   (print-str (ffi (fun (String) Unit)))
  )

  (begin
    (print-int (+  1   1))
    (print-str (+ '1' '1'))
    (+ 1 '2') ; stops here, Int /= String
  )
)
