const bindings = require('./bindings.js');

module.exports = {
  ...bindings,
  parseSync(...args) {
    const result = bindings.parseSync(...args);
    result.program = JSON.parse(result.program);
    return result;
  },
  async parseAsync(...args) {
    const result = await bindings.parseAsync(...args);
    result.program = JSON.parse(result.program);
    return result;
  },
};
