// Creates a mouse handler that calculates client coordinates before calling the real handler.
function mkClientMouseEvent(handler, elem) {
	return function(evt) {
		var parentOffset = $(elem).offset();
		evt.elemX = evt.pageX - parentOffset.left;
		evt.elemY = evt.pageY - parentOffset.top;
		handler(evt);
	};
}

///////////////////////////////////////////////////////////////////////////////
// class Map
///////////////////////////////////////////////////////////////////////////////
function Map(canvas, numX, numY, cellSize, shipDef) {
	var drag = new MouseDrag(canvas, 4);
	var render = new Render(canvas, numX, numY, cellSize);
	var that = this; // this-reference for callbacks
	// convert between grid and canvas coordinates
	var toC = function(gridCoord) { return gridCoord * cellSize; };
	var toG = function(canvasCoord) { return Math.floor(canvasCoord / cellSize); };

	// PUBLIC PROPERTIES
	this.ships = []
	this.safetyMargin = 1;
	this.shipDef = shipDef;
	this.width = numX * cellSize;
	this.height = numY * cellSize;

	// update canvas
	$(canvas).attr('width', this.width);
	$(canvas).attr('height', this.height);

	// DRAG EVENTS
	// drag.onDragStart.add(function (evt) {

	// });
	drag.onDragging.add(function (evt) {
		shipOX = toG(evt.origin.x);
		shipOY = toG(evt.origin.y);
		curShipX = toG(evt.pos.x);
		curShipY = toG(evt.pos.y);

		var ship = Ship.fromLine(shipOX, shipOY, curShipX, curShipY);
		that.redraw();
		style = that.shipAdmissible(ship) ? render.validShipStyle : render.invalidShipStyle;
		render.drawShip(ship, style);
	});
	drag.onDragged.add(function (evt) {
		shipOX = toG(evt.origin.x);
		shipOY = toG(evt.origin.y);
		curShipX = toG(evt.pos.x);
		curShipY = toG(evt.pos.y);
		var ship = Ship.fromLine(shipOX, shipOY, curShipX, curShipY);
		if(that.shipAdmissible(ship)) {
			that.ships.push(ship);
			that.shipDef.takeShip(ship.Size);
		} else if(s = that.shipSelected(ship)) {
			that.ships.splice(that.ships.indexOf(s),1);
			that.shipDef.returnShip(s.Size);
		}
		that.redraw();
	});
	drag.onCancelDrag.add(function () {
		that.redraw();
	});
	drag.onClick.add(function (evt) {
		shipOX = toG(evt.pos.x);
		shipOY = toG(evt.pos.y);
		for (var i = that.ships.length - 1; i >= 0; i--) {
			if(that.ships[i].contains(shipOX, shipOY, 0)) {
				that.shipDef.returnShip(that.ships[i].Size);
				that.ships.splice(i, 1);
			}
		};
		that.redraw();
	});

	this.shipAdmissible = function (ship) {
		for (var i = this.ships.length - 1; i >= 0; i--) {
			if(ship.intersects(this.ships[i], this.safetyMargin)) {
				return false;
			}
		};
		return shipDef.hasShip(ship.Size);
	};
	this.redraw = function() {
		render.clear();
		for (var i = this.ships.length - 1; i >= 0; i--) {
			render.drawShipMargin(this.ships[i], this.safetyMargin);
			render.drawShip(this.ships[i], render.placedShipStyle);
		};
	};
	this.reset = function() {
		this.ships.splice(0, this.ships.length);
		this.shipDef.reset();
		this.redraw();
	};
	this.shipSelected = function (ship) {
		for (var i = this.ships.length - 1; i >= 0; i--) {
			if (ship.equals(this.ships[i])) {
				return this.ships[i];
			}
		}
		return null;
	}

	// initial redraw
	this.redraw();
}


///////////////////////////////////////////////////////////////////////////////
// class MouseDrag
///////////////////////////////////////////////////////////////////////////////
function MouseDrag(elem, threshold) {
	var startDragging = false;
	var dragging = false;

	// define events
	this.onClick = new CustomEvent();
	this.onDragStart = new CustomEvent();
	this.onDragging = new CustomEvent();
	this.onDragged = new CustomEvent();
	this.onCancelDrag = new CustomEvent();

	var dragOrigin;
	var that = this;

	var onMouseDown = function(evt) {
		dragOrigin = {x: evt.elemX, y:evt.elemY};
		startDragging = true;
		evt.preventDefault();
	};
	var onMouseMove = function(evt) {
		if(startDragging) {
			var dx = evt.elemX - dragOrigin.x;
			var dy = evt.elemY - dragOrigin.y;
			if(dx * dx + dy * dy >= threshold * threshold) {
				startDragging = false;
				dragging = true;
				that.onDragStart.raise({origin: dragOrigin});
			}
		} else if(dragging) {
			that.onDragging.raise({origin: dragOrigin, pos: {x:evt.elemX, y: evt.elemY}});
		}

		evt.preventDefault();
	};
	var onMouseUp = function(evt) {
		if(dragging) {
			that.onDragged.raise({origin: dragOrigin, pos: {x:evt.elemX, y: evt.elemY}});
		} else if(startDragging) {
			that.onClick.raise({pos: {x:evt.elemX, y: evt.elemY}});
		}
		dragging = false;
		startDragging = false;
		evt.preventDefault();
	};
	var onMouseLeave = function () {
		if(dragging) {
			that.onCancelDrag.raise();
		}
		startDragging = false;
		dragging = false;
	}

	// register events
	$(elem).mousedown(mkClientMouseEvent(onMouseDown, elem));
	$(elem).mouseup(mkClientMouseEvent(onMouseUp, elem));
	$(elem).mousemove(mkClientMouseEvent(onMouseMove, elem));
	$(elem).mouseleave(onMouseLeave);
}

///////////////////////////////////////////////////////////////////////////////
// class CustomEvent
///////////////////////////////////////////////////////////////////////////////
function CustomEvent() {
	this.callbacks = [];
}
CustomEvent.prototype.add = function (handler) {
	this.callbacks.push(handler);
};
CustomEvent.prototype.remove = function (handler) {
	var i = this.callbacks.indexOf(handler);
	if(i >= 0) {
		this.callbacks.splice(i, 1);
	}
};
CustomEvent.prototype.raise = function (arg) {
	for (var i = this.callbacks.length - 1; i >= 0; i--) {
		this.callbacks[i](arg);
	};
}


///////////////////////////////////////////////////////////////////////////////
// class Render
///////////////////////////////////////////////////////////////////////////////
function Render(canvas, numX, numY, cellSize) {
	var c2d = canvas.getContext("2d");

	// convert between grid and canvas coordinates
	var toC = function(gridCoord) { return gridCoord * cellSize; };
	var toG = function(canvasCoord) { return Math.floor(canvasCoord); };

	// calculate pixel size of map
	var width = toC(numX);
	var height = toC(numY);

	// resize canvas element
	$(canvas).attr('width', width);
	$(canvas).attr('height', height);

	// ship styles
	this.validShipStyle   = { thickness: 1, border: 'rgb(0, 200, 0)', fill : '#AEF100' };
	this.invalidShipStyle = { thickness: 1, border: 'rgb(255, 0, 0)', fill : '#FD0006' };
	this.placedShipStyle  = { thickness: 1, border: 'rgb(0, 0, 200)', fill : '#AEF100' };
	this.blockedCellColor = 'rgba(200, 200, 200, 0.5)';
	// grid style
	this.gridStyle        = { thickness: 2, color: '#D2F870'};

	// rendering functions
	this.clear = function () {
		c2d.clearRect(0, 0, width, height);
	};
	this.drawGrid = function () {
		c2d.save();
		// draw map grid
		c2d.beginPath();
		c2d.strokeStyle = this.gridStyle.color;
		c2d.lineWidth = this.gridStyle.thickness;
		for(var x = 0; x <= width; x += cellSize) {
			c2d.moveTo(x, 0);
			c2d.lineTo(x, height);
		}
		for(var y = 0; y <= height; y += cellSize) {
			c2d.moveTo(0, y);
			c2d.lineTo(width, y);
		}
		c2d.stroke();
		c2d.restore();
	}
	this.drawShip = function (ship, style) {
		c2d.save();

		c2d.strokeStyle = style.border;
		c2d.lineWidth = style.thickness;
		c2d.fillStyle = style.fill;

		var rx = toC(ship.X);
		var ry = toC(ship.Y);
		var rw = toC(ship.Width);
		var rh = toC(ship.Height);
		c2d.fillRect(rx, ry, rw, rh);
		c2d.strokeRect(rx, ry, rw, rh);

		c2d.restore();
	};
	this.drawShipMargin = function (ship, margin) {
		c2d.save();
		c2d.fillStyle = this.blockedCellColor;
		var rx = toC(ship.X - margin);
		var ry = toC(ship.Y - margin);
		var rw = toC(ship.Width + 2 * margin);
		var rh = toC(ship.Height + 2 * margin);
		c2d.fillRect(rx, ry, rw, rh);
		c2d.restore();
	};
}


///////////////////////////////////////////////////////////////////////////////
// class ShipDef
///////////////////////////////////////////////////////////////////////////////
function ShipDef(shipDefOrig) {
	var shipDef = {};
	this.shipTypes = [];
	for(var len in shipDefOrig) {
		shipDef[len] = shipDefOrig[len];
		this.shipTypes.push(parseInt(len));
	}
	
	this.onChanged = new CustomEvent();

	this.hasShip = function (length) {
		return shipDef[length] > 0;
	};
	this.shipCount = function(length) {
		return shipDef[length];
	};
	this.total = function() {
		var t = 0;
		for(var len in shipDef) {
			t += shipDef[len];
		}
		return t;
	};
	this.takeShip = function (length) {
		if(this.hasShip(length)) {
			shipDef[length] -= 1;
		}
		this.onChanged.raise(this);
	};
	this.returnShip = function (length) {
		if(shipDef[length] < shipDefOrig[length]) {
			shipDef[length] += 1;
		}
		this.onChanged.raise(this);
	};
	this.reset = function () {
		for(var len in shipDefOrig) {
			shipDef[len] = shipDefOrig[len];
		}
		this.onChanged.raise(this);
	};
}
ShipDef.fromList = function(shipList) {
	var shipDefOrig = {};
	for (var i = shipList.length - 1; i >= 0; i--) {
		var len = shipList[i];
		if(len in shipDefOrig)
			shipDefOrig[len] += 1;
		else
			shipDefOrig[len] = 1;
	};
	return new ShipDef(shipDefOrig);
}

///////////////////////////////////////////////////////////////////////////////
// class Ship
///////////////////////////////////////////////////////////////////////////////
function Ship(x, y, size, orientation) {
	this.X = x;
	this.Y = y;
	this.Size = size;
	this.Orientation = orientation;
	if(this.Orientation == 0) {
		this.Width = this.Size;
		this.Height = 1;
	} else {
		this.Width = 1;
		this.Height = this.Size;
	}
	this.contains = function(x,y,margin) {
		return (x >= this.X - margin) && (y >= this.Y - margin) && (x < this.X + this.Width + margin) && (y < this.Y + this.Height + margin);
	};
	this.intersects = function(other, margin) {
		var tr = this.X + this.Width - 1;
		var tb = this.Y + this.Height - 1;
		var or = other.X + other.Width + margin - 1;
		var ob = other.Y + other.Height + margin - 1;
		return (this.X <= or &&
				other.X - margin <= tr &&
				this.Y <= ob &&
				other.Y - margin <= tb);
	};
	this.equals = function(other) {
		return (this.X == other.X && 
			    this.Y == other.Y && 
			    this.Orientation == other.Orientation);
	}
}
Ship.fromLine = function(x1, y1, x2, y2) {
	if(Math.abs(x2 - x1) >= Math.abs(y2 - y1)) {
		y2 = y1;
		if(x2 < x1) {
			var t = x1;
			x1 = x2;
			x2 = t;
		}
		var shipSize = Math.abs(x2 - x1) + 1;
		var orientation = 0;
	} else {
		x2 = x1;
		if(y2 < y1) {
			var t = y1;
			y1 = y2;
			y2 = t;
		}
		var shipSize = Math.abs(y2 - y1) + 1;
		var orientation = 1;
	}
	return new Ship(x1, y1, shipSize, orientation);
}