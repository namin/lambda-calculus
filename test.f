order full;
      
zero = lambda s.lambda z.z;
one = lambda s.lambda z.s z;
two = lambda s.lambda z.s (s z);
three = lambda s.lambda z.s (s (s z));

succ = lambda n.lambda s.lambda z.s (n s z);
plus = lambda n.lambda m.lambda s.lambda z.n s (m s z);
times = lambda n.lambda m.lambda s.lambda z.n (m s) z;

trace on;
plus two three;
trace off;

order normal;
trace on;
plus two three;
trace off;

order applicative;
trace on;
plus two three;
trace off;

order full;
      
times two three;

true = lambda x.lambda y.x;
false = lambda x.lambda y.y;

if = lambda p.lambda a.lambda b.p a b;

pair = lambda x.lambda y.lambda f.f x y;
fst = lambda p.p true;
snd = lambda p.p false;

pred = lambda n.fst (n (lambda p.pair (snd p) (succ (snd p))) (pair zero zero));

pred three;

iszero = lambda n.n (lambda x.false) true;

fct = lambda f.lambda n.if (iszero n) one (times n (f (pred n)));

fct_direct = lambda n.n (lambda x.pair (plus (fst x) one) (times (snd x) plus (fst x) one)) (pair zero one);

snd (fct_direct three);

order applicative;

yapp = lambda fun.(lambda f.(f f)) (lambda f.fun (lambda x.(f f) x));
      
yappfct = yapp fct;

rapp = yappfct three;

order full;

rapp;

order normal;

y = lambda f.(lambda x.f (x x)) (lambda x.f (x x));

yfct = y fct;

r = yfct three;

order full;

r;

order applicative;

dummy = lambda x.x;

fct2 = lambda f.lambda n.if (iszero n) (lambda _.one) (lambda _.times n (f (pred n) dummy));

yfct2 = yapp fct2;

r2 = yfct2 three dummy;

order full;

r2;

/*
order applicative;

yfct3 = yapp fct;

r3 = yfct3 three;

order full;

r3;
*/

lambda x. x;

lambda x.lambda f.f x;

lambda x.lambda f.lambda g.g (f x);
