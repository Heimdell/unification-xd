
(let
  ((+     (ffi (fun (((#Map #Int) #String) ((#Map #Int) #String)) ((#Map #Int) #String))))
   (index (ffi (fun (k (#Map k v)) v)))
   (bad   (ffi (fun (#Int (#Map #Int)) #String)))
  )

  (index 0 (+ (map) (map)))
)
