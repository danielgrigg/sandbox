import QtQuick 1.0
import QtDesktop 0.1

Rectangle {

    width: 538 + frame.margins * 2
    height: 360 + frame.margins * 2

    ToolBar{
        id: toolbar
        width: parent.width
        height: 40

        MouseArea {
            anchors.fill:  parent
            acceptedButtons: Qt.RightButton
            onPressed: editmenu.show(mouseX, mouseY)
        }

        ChoiceList {
            id: delegateChooser
            enabled: frame.current == 4 ? 1 : 0
            model: delegatemenu
            anchors.left: parent.left
            anchors.leftMargin: 8
            anchors.verticalCenter: parent.verticalCenter
        }

        CheckBox {
            id: enabledCheck
            text: "Enabled"
            checked: true
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter
        }
    }

    SystemPalette {id: syspal}
    QStyleItem{ id: styleitem}
    color: syspal.window

    XmlListModel {
        id: flickerModel
        source: "http://api.flickr.com/services/feeds/photos_public.gne?format=rss2&tags=" + "Qt"
        query: "/rss/channel/item"
        namespaceDeclarations: "declare namespace media=\"http://search.yahoo.com/mrss/\";"
        XmlRole { name: "title"; query: "title/string()" }
        XmlRole { name: "imagesource"; query: "media:thumbnail/@url/string()" }
        XmlRole { name: "credit"; query: "media:credit/string()" }
    }

    ListModel {
        id: nestedModel
        ListElement{attributes:  ListElement { description: "Core" ; color:"#ffaacc"}}
        ListElement{attributes: ListElement { description: "Second" ; color:"#ffccaa"}}
        ListElement{attributes: ListElement { description: "Third" ; color:"#ffffaa"}}
    }

    ListModel {
        id: largeModel
        Component.onCompleted: {
            for (var i=0 ; i< 5000 ; ++i)
                largeModel.append({"name":"Person "+i , "age": Math.round(Math.random()*100), "gender": Math.random()>0.5 ? "Male" : "Female"})
        }
    }

    Column {
        anchors.top: toolbar.bottom
        anchors.right: parent.right
        anchors.left: parent.left
        anchors.bottom:  parent.bottom
        anchors.margins: 8

        TabFrame {
            id:frame
            focus:true
            enabled: toolbar.enabled
            tabbar: TabBar{parent: frame}

            property int margins : styleitem.style == "mac" ? 16 : 0
            height: parent.height - 34
            anchors.right: parent.right
            anchors.left: parent.left
            anchors.margins: margins

            Tab {
                title: "XmlListModel"

                TableView {
                    model: flickerModel
                    anchors.fill: parent
                    anchors.margins: 12

                    TableColumn {
                        property: "title"
                        caption: "Title"
                        width: 120
                    }
                    TableColumn {
                        property: "credit"
                        caption: "Credit"
                        width: 120
                    }
                    TableColumn {
                        property: "imagesource"
                        caption: "Image source"
                        width: 200
                        visible: true
                    }

                    frame: frameCheckbox.checked
                    headerVisible: headerCheckbox.checked
                    sortIndicatorVisible: sortableCheckbox.checked
                    alternateRowColor: alternateCheckbox.checked
                }
            }
            Tab {
                title: "DirModel"

                TableView {
                    model: FileSystemModel{}
                    anchors.fill: parent
                    anchors.margins: 12

                    TableColumn {
                        property: "fileName"
                        caption: "File Name"
                        width: 120
                    }
                    TableColumn {
                        property: "filePath"
                        caption: "File Path"
                        width: 120
                    }
                    TableColumn {
                        property: "fileSize"
                        caption: "Image source"
                        width: 200
                        visible: true
                    }

                    frame: frameCheckbox.checked
                    headerVisible: headerCheckbox.checked
                    sortIndicatorVisible: sortableCheckbox.checked
                    alternateRowColor: alternateCheckbox.checked
                }
            }
            Tab {
                title: "Multivalue "

                TableView {
                    model: nestedModel
                    anchors.fill: parent
                    anchors.margins: 12

                    TableColumn {
                        property: "attributes"
                        caption: "Text and Color"
                        width: 220
                    }

                    itemDelegate:
                    Item {
                        Rectangle{
                            color: itemValue.get(0).color
                            anchors.top:parent.top
                            anchors.right:parent.right
                            anchors.bottom:parent.bottom
                            anchors.margins: 4
                            width:32
                            border.color:"#666"
                        }
                        Text {
                            width: parent.width
                            anchors.margins: 4
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            elide: itemElideMode
                            text: itemValue.get(0).description
                            color: itemForeground
                        }
                    }

                    frame: frameCheckbox.checked
                    headerVisible: headerCheckbox.checked
                    sortIndicatorVisible: sortableCheckbox.checked
                    alternateRowColor: alternateCheckbox.checked
                }
            }
            Tab {
                title: "Generated"

                TableView {
                    model: largeModel
                    anchors.margins: 12
                    anchors.fill: parent
                    TableColumn {
                        property: "name"
                        caption: "Name"
                        width: 120
                    }
                    TableColumn {
                        property: "age"
                        caption: "Age"
                        width: 120
                    }
                    TableColumn {
                        property: "gender"
                        caption: "Gender"
                        width: 120
                    }
                    frame: frameCheckbox.checked
                    headerVisible: headerCheckbox.checked
                    sortIndicatorVisible: sortableCheckbox.checked
                    alternateRowColor: alternateCheckbox.checked
                }
            }

            Tab {
                title: "Delegates"

                ListModel {
                    id: delegatemenu
                    ListElement { text: "Shiny delegate" }
                    ListElement { text: "Scale selected" }
                    ListElement { text: "Editable items" }
                }

                Component {
                    id: delegate1
                    Item {
                        clip: true
                        Text {
                            width: parent.width
                            anchors.margins: 4
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            elide: itemElideMode
                            text: itemValue ? itemValue : ""
                            color: itemForeground
                        }
                    }
                }

                Component {
                    id: slickRowDelegate
                    Rectangle{
                        color: itemAlternateBackground ? "#cef" : "white"
                        //                            selected: itemSelected ? "true" : "false"
                    }

                }

                Component {
                    id: delegate2
                    Item {
                        height: itemSelected? 60 : 20
                        Behavior on height{ NumberAnimation{}}
                        Text {
                            width: parent.width
                            anchors.margins: 4
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            elide: itemElideMode
                            text: itemValue ? itemValue : ""
                            color: itemForeground
                        }
                    }
                }

                Component {
                    id: editableDelegate
                    Item {
                        Text {
                            width: parent.width
                            anchors.margins: 4
                            anchors.left: parent.left
                            anchors.verticalCenter: parent.verticalCenter
                            elide: itemElideMode
                            text: itemValue ? itemValue : ""
                            color: itemForeground
                            visible: !itemSelected
                        }
                        Loader { // Initialize text editor lazily to improve performance
                            anchors.fill: parent
                            anchors.margins: 4
                            property string modelText: itemValue
                            property string editorText: item ? item.text : itemValue
                            onEditorTextChanged: model.setProperty(rowIndex, itemProperty, editorText)
                            sourceComponent: itemSelected ? editor : null
                            Component {id: editor ; TextInput{ color: itemForeground ; text: modelText} }
                        }
                    }
                }
                TableView {
                    model: largeModel
                    anchors.margins: 12
                    anchors.fill:parent
                    frame: frameCheckbox.checked
                    headerVisible: headerCheckbox.checked
                    sortIndicatorVisible: sortableCheckbox.checked
                    alternateRowColor: alternateCheckbox.checked

                    TableColumn {
                        property: "name"
                        caption: "Name"
                        width: 120
                    }
                    TableColumn {
                        property: "age"
                        caption: "Age"
                        width: 120
                    }
                    TableColumn {
                        property: "sex"
                        caption: "Sex"
                        width: 120
                    }

                    headerDelegate: BorderImage{
                        source: "images/header.png"
                        border{left:2;right:2;top:2;bottom:2}
                        Text {
                            text: itemValue
                            anchors.centerIn:parent
                            color:"#333"
                        }
                    }

                    rowDelegate: Rectangle {
                        color: itemSelected ? "#448" : (itemAlternateBackground ? "#eee" : "#fff")
                        border.color:"#ccc"
                        border.width: 1
                        anchors.left: parent.left
                        anchors.leftMargin: -2
                        anchors.rightMargin: -1
                        BorderImage{
                            id:selected
                            anchors.fill: parent
                            source: "images/selectedrow.png"
                            visible: itemSelected
                            border{left:2;right:2;top:2;bottom:2}
                            SequentialAnimation {
                                running: true; loops: Animation.Infinite
                                NumberAnimation { target:selected; property: "opacity"; to: 1.0; duration: 900}
                                NumberAnimation { target:selected; property: "opacity"; to: 0.5; duration: 900}
                            }
                        }
                    }

                    itemDelegate: {
                        switch(delegateChooser.currentIndex) {
                        case 0:
                            return delegate1
                        case 1:
                            return delegate2
                        case 2:
                            return editableDelegate
                        }
                    }
                }
            }
        }
        Row{
            x:12
            height: 34
            CheckBox{
                id: alternateCheckbox
                checked: true
                text: "Alternate"
                anchors.verticalCenter: parent.verticalCenter
            }
            CheckBox{
                id: sortableCheckbox
                checked: false
                text: "Sortindicator"
                anchors.verticalCenter: parent.verticalCenter
            }
            CheckBox{
                id: frameCheckbox
                checked: true
                text: "Frame"
                anchors.verticalCenter: parent.verticalCenter
            }
            CheckBox{
                id: headerCheckbox
                checked: true
                text: "Headers"
                anchors.verticalCenter: parent.verticalCenter
            }
        }
    }
}
