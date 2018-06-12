module.exports = function (opts) {
  opts = opts || { sep: '{}' }

  var left = opts.sep[0]
  var right = opts.sep[1]

  return function (template, values) {
    Object.keys(values).forEach(function (key) {
      template = template.replace(regExp(key), values[key])
    })
    return template
  }

  function regExp (key) {
    return new RegExp('\\' + left + key + '\\' + right, 'g')
  }
}
