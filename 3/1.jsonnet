local Group(digits) = "ИКБО-" + digits;

local groups = [
  Group(i + "-20")
  for i in std.range(1, 24)
] + [
  Group("10-23"),
];

local Student(age, groupId, name) = {
  age: age,
  group: groups[groupId],
  name: name,
};

{
  groups: groups,
  students: [
    Student(19, 3, "Иванов И.И."),
    Student(18, 4, "Петров П.П."),
    Student(18, 4, "Сидоров С.С."),
    Student(19, std.length(groups) - 1, "Бузин Б.А.")
  ],
  subject: "Конфигурационное управление",
}
