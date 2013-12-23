/********************************************************************************
** Form generated from reading UI file 'irrwidget.ui'
**
** Created: Thu Jul 7 17:43:59 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_IRRWIDGET_H
#define UI_IRRWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHeaderView>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_IrrWidget
{
public:

    void setupUi(QWidget *IrrWidget)
    {
        if (IrrWidget->objectName().isEmpty())
            IrrWidget->setObjectName(QString::fromUtf8("IrrWidget"));
        IrrWidget->resize(400, 300);

        retranslateUi(IrrWidget);

        QMetaObject::connectSlotsByName(IrrWidget);
    } // setupUi

    void retranslateUi(QWidget *IrrWidget)
    {
        IrrWidget->setWindowTitle(QApplication::translate("IrrWidget", "IrrWidget", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class IrrWidget: public Ui_IrrWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_IRRWIDGET_H
