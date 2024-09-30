const { ls } = require('npm-remote-ls');

function toDot(upper, tree) {
  const result = [];

  for (const package in tree) {
    result.push(`"${package}" -- "${upper}"`);
    result.push(toDot(package, tree[package]));
  }

  return result.join('\n');
}

ls('express', 'latest', (tree) => {
  console.log(
    Object.entries(tree)
          .map(([key, val]) => toDot(key, val))
          .join('\n'),
  );
});
