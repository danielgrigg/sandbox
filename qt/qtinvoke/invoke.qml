import QtQuick 1.0

Rectangle {
  width: 100
  height: 62
  MouseArea {
    anchors.fill: parent
    onClicked: {
      myObject.cppMethod("Hello from QML")
      myObject.cppSlot(12345)
    }
  }

  function myQmlFunction(msg) {
    console.log("Got message:", msg)
    return "some return value"
  }
}
