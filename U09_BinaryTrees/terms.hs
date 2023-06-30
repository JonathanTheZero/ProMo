data Operation = Times | Plus | Minus | Division deriving (Show)
data Sign = Positive | Negate deriving (Show)
data Term a = C a | BinaryTerm Operation (Term a) (Term a) | UnaryTerm Sign (Term a) deriving (Show)

x :: Fractional a => Term a
x =
    BinaryTerm
        Times
        (BinaryTerm Plus (C 5) (C 4))
        (BinaryTerm Minus (C 3) (C 2))

operator :: Fractional a => Operation -> a -> a -> a
operator Times = (*)
operator Plus = (+)
operator Minus = (-)
operator Division = (/) -- rauslassen bei Integral

eval :: Fractional a => Term a -> a
eval (C x) = x
eval (UnaryTerm Negate t) = (-1) * eval t
eval (BinaryTerm op a b) = (operator op) (eval a) (eval b) --special case mit eval t1 `div` eval t2

y :: Fractional a => Term a
y =
    BinaryTerm
        Times
        (BinaryTerm Plus (C 5) (UnaryTerm Negate (C 4)))
        (BinaryTerm Minus (C 3) (C 2))

simplify :: Term a -> Term a
simplify (UnaryTerm Negate (UnaryTerm Negate t)) = t
simplify (BinaryTerm Plus t1 (UnaryTerm Negate t2)) = BinaryTerm Minus t1 t2
simplify (BinaryTerm Minus t1 (UnaryTerm Negate t2)) = BinaryTerm Plus t1 t2
simplify (BinaryTerm op a b) = BinaryTerm op (simplify a) (simplify b)

z = UnaryTerm Negate (UnaryTerm Negate (C 5))
z2 = simplify z