#ifndef GRAPHICSVIEW_H
#define GRAPHICSVIEW_H

#include <QGraphicsView>
#include <QResizeEvent>

class GraphicsView : public QGraphicsView
{
public:
  GraphicsView()
  {
    setWindowTitle(tr("doldraw"));
  }

protected:
  void resizeEvent(QResizeEvent *event) {
    if (scene())
      scene()->setSceneRect(QRect(QPoint(0, 0), event->size()));
    QGraphicsView::resizeEvent(event);
  }
};
#endif // GRAPHICSVIEW_H
