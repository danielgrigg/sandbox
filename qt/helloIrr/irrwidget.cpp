#include "irrwidget.h"

IrrWidget::IrrWidget( const QString &windowTitle )
{
    this->resize( 640, 480 );
    this->setWindowTitle( windowTitle );
    this->setAttribute( Qt::WA_PaintOnScreen, true );

    QPushButton *button = new QPushButton( this );
    button->setText( "Quit!" );
    button->move( 20, 20 );

    connect( button, SIGNAL( clicked() ), this, SLOT( quit() ));

    createIrrlichtDevice();
}

void IrrWidget::createIrrlichtDevice()
{
    dimension2d<u32> windowSize( this->geometry().width(), this->geometry().height() );

    qDebug() << "IrrWidget::createIrrlichtDevice, width = " << windowSize.Width << " height = " << windowSize.Height;

    SIrrlichtCreationParameters createParams;
    createParams.WindowId = ( void * ) this->winId();

    m_Device = createDeviceEx( createParams );
    if( m_Device == 0 )
        qDebug() << "failed to create irrlicht device";

    m_Driver = m_Device->getVideoDriver();
    m_Scene = m_Device->getSceneManager();

    buildIrrlichtScene();
}

void IrrWidget::buildIrrlichtScene()
{
    m_Scene->addMeshSceneNode( m_Scene->getMesh("/Users/daniel/sandbox/qt/helloIrr/models/cow.x" ));
    ILightSceneNode *light = m_Scene->addLightSceneNode();
    light->setLightType( ELT_DIRECTIONAL );
    light->setRotation( vector3df( 45.0f, 45.0f, 0.0f ));
    light->getLightData().AmbientColor = SColorf( 0.2f, 0.2f, 0.2f, 1.0f );
    light->getLightData().DiffuseColor = SColorf( 0.8f, 0.8f, 0.8f, 1.0f );

    m_Scene->addCameraSceneNode( 0, vector3df( 6, 6, -6 ), vector3df() );
}

void IrrWidget::paintEvent( QPaintEvent *event )
{
    qDebug() << "IrrWidget::paintEvent()";

    drawIrrlichtScene();
}

void IrrWidget::resizeEvent( QResizeEvent *event )
{
    qDebug() << "IrrWidget::resizeEvent()";
}

QPaintEngine * IrrWidget::paintEngine() const
{
    qDebug() << "IrrWidget::paintEngine()";

    return 0;
}

void IrrWidget::drawIrrlichtScene()
{
    qDebug() << "IrrWidget::drawIrrlichtScene()";

    m_Driver->beginScene( true, false, SColor( 255, 128, 128, 128 ));
    m_Scene->drawAll();
    m_Driver->endScene();
}

void IrrWidget::quit()
{
    this->close();
}

