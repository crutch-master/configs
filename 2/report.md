# Задача 4

```mzn
include "alldifferent.mzn";

array[1..6] of var 0..9: digits;
var int: sumleft = sum (i in 1..3) (digits[i]);
var int: sumright = sum (i in 4..6) (digits[i]);

constraint alldifferent(digits);
constraint sumleft = sumright;

solve minimize sumleft;
output [
  "ticket=\(digits)\n",
  "left=\(sumleft)\n",
  "right=\(sumright)\n"
];
```
