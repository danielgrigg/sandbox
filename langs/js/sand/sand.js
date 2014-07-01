document.writeln('Hello, world');

var nums = [
1, 2, 3 ];

var i;
for (i = 0; i < nums.length; i += 1) {
  document.writeln('nums[' + i + ']: ' + nums[i]);
}

var simpleBatches = [
{position : 2, normal: 3},
{position : 5, normal: 6},
{position : 7, normal: 8}];

for (i = 0; i < simpleBatches.length; i += 1) {
  document.write('simpleBatches[' + i + '].position: ' + simpleBatches[i].position);
  document.writeln('\tsimpleBatches[' + i + '].normal: ' + simpleBatches[i].normal);
}

var batches = 
[
  { 
    positions : [0, 0, 0, 1, 0, 0, 0, 1, 0],
    normals : [0, 0, 1, 0, 0, 1, 0, 0, 1] 
  },
  { 
    positions : [0, 1, 0, 1, 0, 0, 1, 1, 0],
    normals : [0, 0, -1, 0, 0, -1, 0, 0, -1] 
  }
];

document.writeln('batches:\n');


var j;
for (i = 0; i < batches.length; i += 1) {
  document.writeln('batches[' + i + "]:\n");
  
  for (j = 0; j < batches[i].positions.length; j += 1) {
    document.write(batches[i].positions[j] + '\t');
  }
  document.writeln();

  for (j = 0; j < batches[i].normals.length; j += 1) {
    document.write(batches[i].normals[j] + "\t");
  }
  document.writeln();

}


var prism =
[
{
positions : new Float32Array([ -0.288675, -0.5, -0.5, -0.288675, -0.5, 0.5, -0.288675, 0.5, 0.5, -0.288675, 0.5, -0.5, -0.288675, -0.5, 0.5, 0.57735, -0.5, 0, 0.57735, 0.5, 0, -0.288675, 0.5, 0.5, 0.57735, -0.5, 0, -0.288675, -0.5, -0.5, -0.288675, 0.5, -0.5, 0.57735, 0.5, 0, -0.288675, -0.5, -0.5, 0.57735, -0.5, 0, -0.288675, -0.5, 0.5, -0.288675, 0.5, -0.5, -0.288675, 0.5, 0.5, 0.57735, 0.5, 0] ),
normals : new Float32Array([ -1, 0, -0, -1, 0, -0, -1, 0, -0, -1, 0, -0, 0.5, 0, 0.866026, 0.5, 0, 0.866026, 0.5, 0, 0.866026, 0.5, 0, 0.866026, 0.5, 0, -0.866025, 0.5, 0, -0.866025, 0.5, 0, -0.866025, 0.5, 0, -0.866025, 0, -1, -0, 0, -1, -0, 0, -1, -0, 0, 1, -0, 0, 1, -0, 0, 1, -0] ),
uvs : new Float32Array([ 0.375, 0.3125, 0.458333, 0.3125, 0.458333, 0.68844, 0.375, 0.68844, 0.458333, 0.3125, 0.541667, 0.3125, 0.541667, 0.68844, 0.458333, 0.68844, 0.541667, 0.3125, 0.625, 0.3125, 0.625, 0.68844, 0.541667, 0.68844, 0.421875, 0.020934, 0.65625, 0.15625, 0.421875, 0.291566, 0.421875, 0.979066, 0.421875, 0.708434, 0.65625, 0.84375] ),
elements : new Uint16Array([ 0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 8, 9, 10, 8, 10, 11, 12, 13, 14, 15, 16, 17] )
}
];
for (i = 0; i < prism.length; i += 1) {
  document.writeln('prism[' + i + "]:\n");

  for (j = 0; j < prism[i].positions.length; j += 1) {
    document.write(prism[i].positions[j] + ' ');
  }
  document.writeln();

  for (j = 0; j < prism[i].normals.length; j += 1) {
    document.write(prism[i].normals[j] + " ");
  }
  document.writeln();

}


