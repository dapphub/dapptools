var app = document.querySelector("#app")
var params = {}
var token

onhashchange = function() {
  location.reload()
}

location.hash.replace(/^\#\?/, "").split("&").forEach(function(part) {
  var [name, value] = part.split("=")
  params[name] = value
})

if (!params.token || !params.account) {
  var token = prompt("What is the address of the ERC20 token?")
  var account = prompt("Which account are you interested in?")
  location.hash = `?token=${token}&account=${account}`
  location.reload()
}

var ERC20 = abi([["balanceOf", ["address"], ["uint"]]])

var loading = setTimeout(function() {
  app.innerHTML = `
    <div class=note>
      <h2>Loading token balance...</h2>
      This should not take very long.  If this message does not
      disappear after a few seconds, something might be wrong.
    </div>
  `
}, 500)

onload = function() {
  if (this.web3) {
    load()
  } else {
    clearTimeout(loading)
    app.innerHTML = `
      <div class=note>
        <h2>No web3 provider found</h2>
        Consider installing <a href="https://metamask.io">MetaMask</a>, or
        cloning this repository and running an Ethereum client locally.
      </div>
    `
  }
}

function load() {
  token = web3.eth.contract(ERC20).at(params.token)
  token.balanceOf.call(params.account, hopefully(balance => {
    clearTimeout(loading)
    document.querySelector("#app").innerHTML = `
      <table>
        <tr>
          <th>Token</th>
          <td><code>${params.token}</code></td>
        </tr>
        <tr>
          <th>Account</th>
          <td><code>${params.account}</code></td>
        </tr>
        <tr>
          <th>Balance</th>
          <td><code>${web3.fromWei(balance, "ether")}</code></td>
        </tr>
      </table>
    `
  }))
}

function abi(functions) {
  var params = types => types.map((type, i) => ({ name: "x" + i, type }))
  return functions.map(([name, inputs=[], outputs=[]]) => ({
    name, type: "function",
    inputs: params(inputs),
    outputs: params(outputs),
  }))
}

function hopefully(callback) {
  return function(error, result) {
    if (error) {
      alert(error.message)
    } else {
      callback(result)
    }
  }
}
