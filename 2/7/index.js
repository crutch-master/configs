const { readFile } = require("node:fs");

function readFileAsync(filename) {
  return new Promise((res, rej) =>
    readFile(filename, (err, data) => {
      if (err) rej(err);

      res(data);
    }),
  );
}

async function jsonToPackage(filename) {
  return JSON.parse(await readFileAsync(filename));
}

function solve(packages, targetName) {
  let installed = 0;
  const installOrder = {};

  for (const versions of packages[targetName].deps) {
    const child = versions.find((version) => version in packages);
    const installOrderChild = solve(packages, child);
    let installedChild = 0;

    for (const key in installOrderChild) {
      if (key in installOrder) continue;

      installOrder[key] = installOrderChild[key] + installed;
      installedChild += 1;
    }

    installed += installedChild;

    if (!(child in installOrder)) {
      installOrder[child] = installed;
      installed += 1;
    }
  }

  installOrder[targetName] = installed;
  return installOrder;
}

(async () => {
  const jsons = process.argv.splice(2);
  const packages = {};

  await Promise.all(
    jsons.map(async (filename) => {
      const package = await jsonToPackage(filename);
      packages[package.name] = package;
    }),
  );

  const targetName = (await jsonToPackage(jsons[0])).name;
  const solution = solve(packages, targetName);
  console.log(solution);
})();
