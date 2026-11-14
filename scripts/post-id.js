'use strict';

// Hexo loads every file in scripts/ at startup with `hexo` injected as a global.
// A post created by `hexo new` gets its permalink id written into the front matter
// immediately, so the id never depends on db.json and never changes on a rebuild.

const crypto = require('crypto');
const fs = require('fs');

function freshId() {
  const random = crypto.randomBytes(32).toString('hex').slice(0, 24);
  return `c${random}`;
}

function frontMatterBounds(lines) {
  if (!lines.length || lines[0].trim() !== '---') return null;
  for (let i = 1; i < lines.length; i++) {
    if (lines[i].trim() === '---') return i;
  }
  return null;
}

hexo.on('new', post => {
  try {
    const file = post.path;
    const raw = fs.readFileSync(file);
    const bom = raw.length >= 3 && raw[0] === 0xef && raw[1] === 0xbb && raw[2] === 0xbf;
    const text = (bom ? raw.slice(3) : raw).toString('utf8');
    const newline = text.includes('\r\n') ? '\r\n' : '\n';
    const lines = text.split(/\r?\n/);
    const end = frontMatterBounds(lines);
    if (end === null) return;
    if (lines.slice(1, end).some(line => /^_id\s*:/.test(line))) return;

    const id = freshId();
    lines.splice(end, 0, `_id: ${id}`);
    const out = Buffer.concat([bom ? Buffer.from([0xef, 0xbb, 0xbf]) : Buffer.alloc(0), Buffer.from(lines.join(newline), 'utf8')]);
    fs.writeFileSync(file, out);
    hexo.log.info(`pinned post id ${id} for ${post.path}`);
  } catch (err) {
    hexo.log.warn(`could not pin post id: ${err.message}`);
  }
});
