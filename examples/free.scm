(let
  ( ; data List =
    (newtype #List (a)
      ( ; | Cons { head : a, tail : List a }
        (Cons (rec (head a) (tail (#List a))))

        ; | Nil ()
        (Nil  #Unit)
      )
    )

    ; nil = Nil ()
    (nil  (Nil ()))

    ; cons = \x, xs => Cons { head = x, tail = xs }
    (cons (fun (x xs) (Cons (rec (head x) (tail xs)))))

    ; (+) = ffi "+" : (Int, Int) -> Int
    (+    (ffi (fun (#Int #Int) #Int)))
  )
(let
  ( ; pure = \x => cons x nil
    (pure (fun (x) (cons x nil)))
  )
(let
  (
    (newtype #Free (f a)
      ( (Free (rec (un (f (#Free f a)))))
        (Pure a)
      )
    )

    (free (fun (f) (Free (rec (un f)))))
  )

(let
  (
    (tree (free (pure (Pure 2))))
  )

  tree
))))
