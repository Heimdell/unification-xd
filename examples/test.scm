
(let
  (; identity
   (id    (fun (x) x))

   ; K
   (const (fun (x y) x))

   ; integer plus
   (+     (ffi (fun (Num #Int) #Int)))

   (type Num #Int)

   ; record construction
   (Point (fun (x y) (rec (x x) (y y))))

   ; record polymorphism
   (y     (fun (r) (get y (:: r (rec (x x) (y y))))))

   ; helper to please inference
   (void  (fun (x) ()))
  )

  (begin
    (void ((id id) 1))
    (y (y (Point (Point (+ 1 3) '2') (Point 2 '3'))))
  )
)
