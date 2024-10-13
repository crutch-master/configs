# Задание 1
```jsonnet
local Group(digits) = 'ИКБО-' + digits;

local groups = [
  Group(i + '-20')
  for i in std.range(1, 24)
] + [
  Group('10-23'),
];

local Student(age, groupId, name) = {
  age: age,
  group: groups[groupId],
  name: name,
};

{
  groups: groups,
  students: [
    Student(19, 3, 'Иванов И.И.'),
    Student(18, 4, 'Петров П.П.'),
    Student(18, 4, 'Сидоров С.С.'),
    Student(19, std.length(groups) - 1, 'Бузин Б.А.'),
  ],
  subject: 'Конфигурационное управление',
}
```

![](1.png)

# Задание 2
```dhall
let List/map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v23.0.0/Prelude/List/map

let List/drop =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v23.0.0/Prelude/List/drop

let List/index =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v23.0.0/Prelude/List/index

let Natural/enumerate =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/v23.0.0/Prelude/Natural/enumerate

let Range =
      \(n : Natural) ->
      \(m : Natural) ->
        List/drop n Natural (Natural/enumerate (m + 1))

let Group = \(n : Text) -> "ИКБО-" ++ n

let groups =
        List/map
          Natural
          Text
          (\(i : Natural) -> Group (Natural/show i ++ "-20"))
          (Range 1 24)
      # [ Group "10-23" ]

let Student =
      \(age : Natural) ->
      \(groupId : Natural) ->
      \(name : Text) ->
        { age, group = List/index groupId Text groups, name }

in  { groups
    , students =
      [ Student 19 3 "Иванов И.И."
      , Student 18 4 "Петров П.П."
      , Student 18 4 "Сидоров С.С."
      , Student 19 (Natural/subtract 1 (List/length Text groups)) "Бузин Б.А."
      ]
    }
```

![](2.png)

# Задание 3
```python
```

# Задание 4
```python
```

# Задание 5
```python
```
