# Задача 4

```minizinc
include "alldifferent.mzn";

array[0..5] of var 0..9: digits;
var int: sumleft = sum (i in 0..2) (digits[i]);
var int: sumright = sum (i in 3..5) (digits[i]);

constraint alldifferent(digits);
constraint sumleft = sumright;

solve minimize sumleft;
output [
  "ticket=\(digits)\n",
  "left=\(sumleft)\n",
  "right=\(sumright)\n"
];
```
