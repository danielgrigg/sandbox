#include "window.h"
#include "ui_window.h"

Window::Window(QWidget *parent) :
    QMainWindow(parent)
//    ui(new Ui::Window)
{
//    ui->setupUi(this);

/*
    QGridLayout *mainLayout = new QGridLayout;

    for (int i = 0; i < NumRows; ++i) {
        for (int j = 0; j < NumColumns; ++j) {
            QColor clearColor;
            clearColor.setHsv(((i * NumColumns) + j) * 255
                              / (NumRows * NumColumns - 1),
                              255, 63);

            glWidgets[i][j] = new GLWidget(0, 0);
            glWidgets[i][j]->setClearColor(clearColor);
            glWidgets[i][j]->rotateBy(+42 * 16, +42 * 16, -21 * 16);
            mainLayout->addWidget(glWidgets[i][j], i, j);

            connect(glWidgets[i][j], SIGNAL(clicked()),
                    this, SLOT(setCurrentGlWidget()));
        }
    }
    setLayout(mainLayout);

    currentGlWidget = glWidgets[0][0];

    QTimer *timer = new QTimer(this);
    connect(timer, SIGNAL(timeout()), this, SLOT(rotateOneStep()));
    timer->start(20);

    setWindowTitle(tr("Textures"));
*/
}

Window::~Window()
{
 //   delete ui;
}
