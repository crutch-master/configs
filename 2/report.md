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

# Задача 5

```mzn
enum Package = {
  root,
  menu_1_0_0,
  menu_1_1_0,
  menu_1_2_0,
  menu_1_3_0,
  menu_1_4_0,
  menu_1_5_0,

  dropdown_1_8_0,
  dropdown_2_0_0,
  dropdown_2_1_0,
  dropdown_2_2_0,
  dropdown_2_3_0,

  icons_1_0_0,
  icons_2_0_0,
};

array[1..5] of set of Package: targets = [
  1: { icons_1_0_0 },
  2: { menu_1_0_0, menu_1_5_0 },
  3: { dropdown_1_8_0 },
  4: { dropdown_2_0_0, dropdown_2_3_0 },
  5: { icons_2_0_0 }
];

% set points to targets array
array[Package] of set of 1..5: dependencies = [
  root: { 1, 2 },

  menu_1_0_0: { 3 },
  menu_1_1_0: { 4 },
  menu_1_2_0: { 4 },
  menu_1_3_0: { 4 },
  menu_1_4_0: { 4 },
  menu_1_5_0: { 4 },

  dropdown_1_8_0: {},
  dropdown_2_0_0: { 5 },
  dropdown_2_1_0: { 5 },
  dropdown_2_2_0: { 5 },
  dropdown_2_3_0: { 5 },

  icons_1_0_0: {},
  icons_2_0_0: {},
];

array[Package] of var opt (1..100): install_order;

constraint occurs(install_order[root]);

constraint forall(p in Package where occurs(install_order[p])) (
  forall(dep in dependencies[p]) (
    exists(t in targets[dep]) (
      occurs(install_order[t]) /\
      install_order[t] < install_order[p]
    )
  )
);

output [
  if fix(occurs(install_order[p]))
  then "\(p): \(install_order[p])\n"
  else ""
  endif | p in Package
];
```
