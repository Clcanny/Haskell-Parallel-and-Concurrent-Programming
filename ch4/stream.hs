data IList a = Nil | Cons a (IVar (IList a))
type Stream a = IVar (IList a)

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = 
  new >>= \var ->
  fork $ loop xs var >>
  return var
 where
  loop [] var = put var Nil
  loop (x : xs) = 
    new >>= \tail ->
    put var (Cons x tail) >>
    loop xs tail
