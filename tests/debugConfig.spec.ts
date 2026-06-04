import { describe, it, expect, vi } from 'vitest';
import { resolveDebugArgs } from '../src/extension/debugConfig';
import type { RunArgs } from '../src/shared/util_client';

const mockArgs: RunArgs = { uri: 'file.catala_en', scope: 'MyScope' };

const baseConfig = () => ({
  type: 'catala-debugger',
  request: 'launch',
  name: 'Catala Debug',
  stopOnEntry: true,
});

describe('resolveDebugArgs', () => {
  it('calls selectScope and fills args when scope is missing', async () => {
    const selectScope = vi.fn().mockResolvedValue(mockArgs);
    const result = await resolveDebugArgs(baseConfig(), selectScope);
    expect(selectScope).toHaveBeenCalledOnce();
    expect(result?.args).toEqual(mockArgs);
  });

  it('passes through unchanged when args.scope is already set', async () => {
    const selectScope = vi.fn();
    const config = { ...baseConfig(), args: mockArgs };
    const result = await resolveDebugArgs(config, selectScope);
    expect(selectScope).not.toHaveBeenCalled();
    expect(result).toBe(config);
  });

  it('returns undefined when user cancels scope selection', async () => {
    const selectScope = vi.fn().mockResolvedValue(undefined);
    const result = await resolveDebugArgs(baseConfig(), selectScope);
    expect(result).toBeUndefined();
  });
});
