
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

    ; fold =
    (fold
      ; \op, zero, list =>
      (fun (op zero list)
        (case list
          ; | Cons {head, tail} => ...
          ((Cons (rec head tail)) (op head (fold op zero tail)))
          ; | Nil _ => ...
          ((Nil  u)               zero))
      )
    )
  )
(let ( (append (fun (xs ys) (fold cons ys xs))) )
(let ( (join   (fun (lists) (fold append nil lists))) )
(let ( (two    (fold + 0 (join (pure (pure 2))))) )
  two
)))))
