zero = lambda s.lambda z.z;
one = lambda s.lambda z.s z;
two = lambda s.lambda z.s (s z);
three = lambda s.lambda z.s (s (s z));

succ = lambda n.lambda s.lambda z.s (n s z);
plus = lambda n.lambda m.lambda s.lambda z.n s (m s z);
times = lambda n.lambda m.lambda s.lambda z.n (m s) z;

true = lambda x.lambda y.x;
false = lambda x.lambda y.y;

if = lambda p.lambda a.lambda b.p a b;

pair = lambda x.lambda y.lambda f.f x y;
fst = lambda p.p true;
snd = lambda p.p false;

pred = lambda n.fst (n (lambda p.pair (snd p) (succ (snd p))) (pair zero zero));

iszero = lambda n.n (lambda x.false) true;

order full;
plus two three;

order applicative;
trace on;
plus two three;
trace off;

order normal;
trace on;
plus two three;
trace off;

fct = lambda f.lambda n.if (iszero n) one (times n (f (pred n)));

fct_direct = lambda n.n (lambda x.pair (plus (fst x) one) (times (snd x) plus (fst x) one)) (pair zero one);

order full;
snd (fct_direct three);

order normal;
y = lambda f.(lambda x.f (x x)) (lambda x.f (x x));
yfct = y fct;
r = yfct three;
order full;
r;

order applicative;
yapp = lambda fun.(lambda f.(f f)) (lambda f.fun (lambda x.(f f) x));

dummy = lambda x.x;

fctapp = lambda f.lambda n.if (iszero n) (lambda _.one) (lambda _.times n (f (pred n) dummy));

yappfct = yapp fctapp;

rapp = yappfct three dummy;

order full;

rapp;

lambda x. x;

lambda f.lambda x.f x;

lambda x.lambda f.lambda g.g (f x);
