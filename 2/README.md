# Задача 1

![](1.png)

Напрямую можно склонировать GitHub-репозиторий с помощью команды:
```bash
git clone https://github.com/matplotlib/matplotlib
```

# Задача 2

![](2.png)

Напрямую можно также склонировать с помощью команды:
```bash
git clone https://github.com/expressjs/express
```

# Задача 3

![](graph.png)

Код для генерации `dot`-кода в файле `index.js`.

# Задача 4

```minizinc
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

```minizinc
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

# Задача 6

```minizinc
enum Package = {
  root_1_0_0,

  foo_1_1_0,
  foo_1_0_0,

  left_1_0_0,
  right_1_0_0,

  shared_2_0_0,
  shared_1_0_0,

  target_2_0_0,
  target_1_0_0,
};

array[1..7] of set of Package: targets = [
  % Deps of root 1.0.0
  1: { foo_1_1_0, foo_1_0_0 },
  2: { target_2_0_0 },

  % Deps of foo 1.1.0
  3: { left_1_0_0 },
  4: { right_1_0_0 },

  % Deps of left 1.0.0
  5: { shared_1_0_0, shared_2_0_0 },

  % Deps of right 1.0.0
  6: { shared_1_0_0 },

  % Deps of shared 1.0.0
  7: { target_1_0_0 },
];

% set points to targets array
array[Package] of set of 1..7: dependencies = [
  root_1_0_0: { 1, 2 },
  foo_1_1_0: { 3, 4 },
  left_1_0_0: { 5 },
  right_1_0_0: { 6 },
  shared_1_0_0: { 7 },

  foo_1_0_0: { },
  shared_2_0_0: { },

  target_2_0_0: { },
  target_1_0_0: { },
];

array[Package] of var opt (1..100): install_order;

constraint occurs(install_order[root_1_0_0]);

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

# Задача 7
