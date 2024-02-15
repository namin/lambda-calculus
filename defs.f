order normal;

true = lambda x.lambda y.x;
false = lambda x.lambda y.y;
if = lambda b.lambda t.lambda f.b t f;
not = lambda b.b false true;
and = lambda b1.lambda b2.b1 b2 false;
or = lambda b1.lambda b2.b1 true b2;

zero = lambda f.lambda x.x;
one = lambda f.lambda x.f x;
two = lambda f.lambda x.f (f x);
three = lambda f.lambda x.f (f (f x));
succ = lambda n.lambda f.lambda x.f (n f x);

add = lambda n1.lambda n2.n1 succ n2;

times = lambda n1.lambda n2.n1 (add n2) zero;

iszero = lambda n.n (lambda b.false) true;

pair = lambda x.lambda y.lambda f.f x y;
fst = lambda p.p true;
snd = lambda p.p false;

pred = lambda n.fst (n (lambda p.pair (snd p) (succ (snd p))) (pair zero zero));

