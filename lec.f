order normal;

true = lambda x.lambda y.x;
false = lambda x.lambda y.y;
if = lambda b.lambda t.lambda f.b t f;
not = lambda b.b false true;
and = lambda b1.lambda b2.b1 b2 false;
or = lambda b1.lambda b2.b1 true b2;

and true false;
and true (not true);

zero = lambda f.lambda x.x;
one = lambda f.lambda x.f x;
two = lambda f.lambda x.f (f x);
three = lambda f.lambda x.f (f (f x));
succ = lambda n.lambda f.lambda x.f (n f x);

succ two;

add = lambda n1.lambda n2.n1 succ n2;

add two three;

times = lambda n1.lambda n2.n1 (add n2) zero;

times two three;

iszero = lambda n.n (lambda b.false) true;

iszero zero;

iszero one;

iszero two;

pair = lambda x.lambda y.lambda f.f x y;
fst = lambda p.p true;
snd = lambda p.p false;

pred = lambda n.fst (n (lambda p.pair (snd p) (succ (snd p))) (pair zero zero));

pred two;

facti = lambda f.lambda n. if (iszero n) one (times n (f f (pred n)));

order cbv;

fact = facti facti;

fact3 = fact three;

order normal;
fact3;

order cbn;

factp = lambda f.lambda n.if (iszero n) one (times n (f (pred n)));

y = lambda f.(lambda x.f (x x)) (lambda x.f (x x));

fact = y factp;

fact;

step 5;
order cbv;
trace on;
y factp;

trace off;
order cbn;
step *;

order cbv;
z = lambda f.(lambda x.f (lambda y.x x y)) (lambda x.f (lambda y.x x y));

z factp;

thetai = lambda t.lambda f.f (t t f);

theta = thetai thetai;

order cbn;

theta factp;

order normal;

theta factp three;
