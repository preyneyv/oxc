import * as bindings from './bindings.cjs';
export * from './bindings.cjs';

export function parseSync(...args) {
  const result = bindings.parseSync(...args);
  result.program = JSON.parse(result.program);
  return result;
}

export async function parseAsync(...args) {
  const result = await bindings.parseAsync(...args);
  result.program = JSON.parse(result.program);
  return result;
}
