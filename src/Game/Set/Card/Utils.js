isSet__ = function (card1, card2, card3) {
  for (var attr in card1) {
    var attr1 = card1[attr].constructor.name,
        attr2 = card2[attr].constructor.name,
        attr3 = card3[attr].constructor.name
    var allSame = attr1 == attr2 && attr2 == attr3
    var allDifferent = attr1 !== attr2 && attr2 !== attr3 && attr3 !== attr1
    if (!(allSame || allDifferent)) return false
  }
  return true
}


findSets = function(cards) {
  var foundSets = []
  for (var i = 0; i < cards.length - 2; i++) {
    for (var j = i + 1; j < cards.length - 1; j++) {
      for (var k = j + 1; k < cards.length; k++) {
        var card1 = cards[i], card2 = cards[j], card3 = cards[k]
        if (isSet__(card1, card2, card3))
          foundSets.push([card1, card2, card3])
      }
    }
  }
  console.log("Found these sets:", foundSets)
  return foundSets
}

module.exports = {
  isSet__: isSet__,
  findSets: findSets,
}
