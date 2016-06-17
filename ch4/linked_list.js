var node3 = {
  value: "last",
  next: null
};

var node2 = {
  value: "middle",
  next: node3
};

var node1 = {
  value: "first",
  next: node2
};

var first = function (node) {
  return node.value;
};

var rest = function (node) { // I don't really get how this works, since it only returns one thing
  return node.next;
};

var cons = function (newValue, node) { // this is like an insert-before? does it always need to add to the front of a list?
  return {
    value: newValue,
    next: node
  };
};

console.log(first(node1));
console.log(first(rest(node1)));
console.log((first(rest(rest(node1)))));

console.log("----");
var node0 = cons("new first", node1);
console.log(first(node0));
console.log(first(rest(node0)));


var map = function(list, transform) { // list is really just the starting node, right?
  if (list === null) {
    return null;
  } else {
    return cons(transform(first(list)), map(rest(list), transform)); // eventually returns null, which is the 'next' value for the last one
  }
};

console.log(map(node1, function(x) { return x + " hi"; }));
console.log(first(map(node1, function(x) { return x + " hi"; })));






