'use strict';

// No shebang here on purpose: hexo wraps every file in scripts/ inside a
// function scope, and a leading "#!" line makes that wrapper a syntax error.
// The script is invoked explicitly via `node scripts/pin-ids.js`.

// Assigns a stable `_id` to every post that lacks one.
//
// Hexo stores the post id only in db.json; `hexo clean` (and any build on a
// fresh checkout, such as CI) regenerates a random cuid for posts that do not
// carry one, which silently changes every `:id` permalink. The value written
// here is derived from the post path, so it is identical on every machine and
// on every build.

const crypto = require('crypto');
const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');
const POST_DIR = path.join(ROOT, 'source', '_posts');

function stableId(relativePath) {
  const digest = crypto.createHash('sha1').update(relativePath.split(path.sep).join('/')).digest('hex');
  return `c${digest.slice(0, 24)}`;
}

function walk(dir, out) {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) walk(full, out);
    else if (entry.name.endsWith('.md')) out.push(full);
  }
  return out;
}

function readSource(file) {
  const raw = fs.readFileSync(file);
  const bom = raw.length >= 3 && raw[0] === 0xef && raw[1] === 0xbb && raw[2] === 0xbf;
  return { bom, text: (bom ? raw.slice(3) : raw).toString('utf8') };
}

function writeSource(file, { bom, text }) {
  fs.writeFileSync(file, Buffer.concat([bom ? Buffer.from([0xef, 0xbb, 0xbf]) : Buffer.alloc(0), Buffer.from(text, 'utf8')]));
}

function frontMatterBounds(lines) {
  if (!lines.length || lines[0].trim() !== '---') return null;
  for (let i = 1; i < lines.length; i++) {
    if (lines[i].trim() === '---') return i;
  }
  return null;
}

function main(argv) {
  const checkOnly = argv.includes('--check');
  if (!fs.existsSync(POST_DIR)) {
    console.log(`no post directory at ${POST_DIR}`);
    return 0;
  }

  const files = walk(POST_DIR, []);
  const missing = [];
  let written = 0;

  for (const file of files) {
    const relative = path.relative(ROOT, file);
    const source = readSource(file);
    const newline = source.text.includes('\r\n') ? '\r\n' : '\n';
    const lines = source.text.split(/\r?\n/);
    const end = frontMatterBounds(lines);
    if (end === null) continue;

    const body = lines.slice(1, end);
    if (body.some(line => /^_id\s*:/.test(line))) continue;

    const id = stableId(relative);
    missing.push(`${relative} -> ${id}`);
    if (checkOnly) continue;

    lines.splice(end, 0, `_id: ${id}`);
    writeSource(file, { bom: source.bom, text: lines.join(newline) });
    written += 1;
    console.log(`pinned ${relative} -> ${id}`);
  }

  if (checkOnly && missing.length) {
    console.error(`${missing.length} post(s) without a pinned id:`);
    for (const item of missing) console.error(`  ${item}`);
    return 1;
  }

  console.log(`posts=${files.length} pinned=${written} missing=${missing.length}`);
  return 0;
}

process.exitCode = main(process.argv.slice(2));
