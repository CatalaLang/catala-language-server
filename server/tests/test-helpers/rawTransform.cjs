module.exports = {
  process() {
    return { code: `module.exports = '';` };
  },
  getCacheKey() {
    // The output is always the same, so use a literal cache key.
    return 'rawTransform';
  },
};
