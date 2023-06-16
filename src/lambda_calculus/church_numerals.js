const zero = (f) => (x) => x;
const one = (f) => (x) => f(x);
const plus = (m) => (n) => (f) => (x) => m(f)(n(f)(x));
const succ = (n) => (f) => (x) => plus(one)(n)(f)(x);
const unit = zero;
const true_ = (a) => (_) => a(unit);
const false_ = (_) => (b) => b(unit);
const if_ = (pred) => (a) => (b) => pred(a)(b);
const is_zero = (n) => n((_) => false_)(true_);

const pred = (() => {
  const true_ = (a) => (b) => a;
  const false_ = (a) => (b) => b;
  const pair = (x) => (y) => (f) => f(x)(y);
  const first = (p) => p(true_);
  const second = (p) => p(false_);
  const shift = (p) => pair(second(p))(succ(second(p)));
  return (n) => first(n(shift)(pair(zero)(zero)));
})();


const z = g => (x => g(v => x(x)(v)))(x => g(v => x(x)(v)))

const plus_z = z(recurse => a => b => if_(is_zero(b))((_) => a)((_) => recurse(succ(a))(pred(b))));
const minus_z = z(recurse => a => b => if_(is_zero(b))((_) => a)((_) => recurse(pred(a))(pred(b))));

const church_to_int = (cn) => cn((x) => x + 1)(0);
const print_church = (label, x) => console.log(label + ": " + church_to_int(x));

print_church("zero", zero);
print_church("succ zero", succ(zero));
print_church("pred 1", pred(one));
print_church("plus 1 (succ zero)", plus(one)(succ(zero)));
print_church("pred(plus 1 (succ zero))", pred(plus(one)(succ(zero))));
print_church(
  "if zero test: true",
  if_(is_zero(zero))((_) => one)((_) => zero)
);
print_church(
  "if zero test: false",
  if_(is_zero(one))((_) => one)((_) => zero)
);
print_church(
  "if zero test: false",
  if_(is_zero(succ(zero)))((_) => one)((_) => zero)
);
print_church("plus_z: 1 + 1", plus_z(one)(one));
print_church("minus_z: 2 - 1", minus_z(succ(one))(one));