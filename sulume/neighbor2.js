var N=1000;
var dist2 = function(i, j) {
	return Math.pow(map.sites[i].x - map.sites[j].x, 2) + Math.pow(map.sites[i].y - map.sites[j].y, 2);
}
var conflict = function(s, t) {
	return map.rivers.find(function(r) { return r.source == s && r.target == t || r.source == t && r.target == s })
}
for (var i = 0; i < N; ++i)
	map.sites.push({ 'id': i, 'x': 300 + Math.random() * 200, 'y': 200 + Math.random() * 200 });
for (var i = 0; i < 16; ++i)
	map.mines.push(i);
for (var u = 0; u < 2; ++u) {
	for (var i = 0; i < N; ++i) {
		var c = -1,
			d = Infinity;
		for (var j = 0; j < N; ++j) {
			var e = dist2(i, j);
			if (i != j && e < d && !conflict(i, j)) {
				c = j;
				d = e
			}
		}
		map.rivers.push({ 'source': i, 'target': c });
	}
}
for (var t = 0; t < 40; t++){
for (var i = 0; i < map.sites.length; i++)
for (var j = i+1; j < map.sites.length; j++)
{
	var dx, dy, d2 = dist2(i,j);
	dx = (map.sites[i].x - map.sites[j].x) / d2;
	dy = (map.sites[i].y - map.sites[j].y) / d2;
	map.sites[i].x += dx;
	map.sites[i].y += dy;
	map.sites[j].x -= dx;
	map.sites[j].y -= dy;
}
}
refreshIndices();
output();
draw();
