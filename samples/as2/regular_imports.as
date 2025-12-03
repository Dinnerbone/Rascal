import flash.display.BitmapData;
import flash.geom.Point;

trace(BitmapData);
trace(Point);

var myBitmap = new BitmapData(100, 100);
trace(myBitmap);

var myPoint = new Point(10, 20);
trace(myPoint);

trace(BitmapData == flash.display.BitmapData);

function createPoint() {
    return new Point(5, 5);
}
trace(createPoint());
