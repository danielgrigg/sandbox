var positions = new Float32Array([ 0, 0, 0, 10, 0, 0, 10, 10, 0, 0, 10, 0, 20, 0, 0, 20, 10, 0, 30, 0, 0, 30, 10, 0, 40, 0, 0, 40, 10, 0, 50, 0, 0, 50, 10, 0, 60, 0, 0, 60, 10, 0, 70, 0, 0, 70, 10, 0, 80, 0, 0, 80, 10, 0, 90, 0, 0, 90, 10, 0, 100, 0, 0, 100, 10, 0, 10, 20, 0, 0, 20, 0, 20, 20, 0, 30, 20, 0, 40, 20, 0, 50, 20, 0, 60, 20, 0, 70, 20, 0, 80, 20, 0, 90, 20, 0, 100, 20, 0, 10, 30, 0, 0, 30, 0, 20, 30, 0, 30, 30, 0, 40, 30, 0, 50, 30, 0, 60, 30, 0, 70, 30, 0, 80, 30, 0, 90, 30, 0, 100, 30, 0, 10, 40, 0, 0, 40, 0, 20, 40, 0, 30, 40, 0, 40, 40, 0, 50, 40, 0, 60, 40, 0, 70, 40, 0, 80, 40, 0, 90, 40, 0, 100, 40, 0, 10, 50, 0, 0, 50, 0, 20, 50, 0, 30, 50, 0, 40, 50, 0, 50, 50, 0, 60, 50, 0, 70, 50, 0, 80, 50, 0, 90, 50, 0, 100, 50, 0, 10, 60, 0, 0, 60, 0, 20, 60, 0, 30, 60, 0, 40, 60, 0, 50, 60, 0, 60, 60, 0, 70, 60, 0, 80, 60, 0, 90, 60, 0, 100, 60, 0, 10, 70, 0, 0, 70, 0, 20, 70, 0, 30, 70, 0, 40, 70, 0, 50, 70, 0, 60, 70, 0, 70, 70, 0, 80, 70, 0, 90, 70, 0, 100, 70, 0, 10, 80, 0, 0, 80, 0, 20, 80, 0, 30, 80, 0, 40, 80, 0, 50, 80, 0, 60, 80, 0, 70, 80, 0, 80, 80, 0, 90, 80, 0, 100, 80, 0, 10, 90, 0, 0, 90, 0, 20, 90, 0, 30, 90, 0, 40, 90, 0, 50, 90, 0, 60, 90, 0, 70, 90, 0, 80, 90, 0, 90, 90, 0, 100, 90, 0, 10, 100, 0, 0, 100, 0, 20, 100, 0, 30, 100, 0, 40, 100, 0, 50, 100, 0, 60, 100, 0, 70, 100, 0, 80, 100, 0, 90, 100, 0, 100, 100, 0 ]);
var normals = new Float32Array([ 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1 ]);
var uvs = new Float32Array([ 0, 0, 0.1, 0, 0.1, 0.1, 0, 0.1, 0.2, 0, 0.2, 0.1, 0.3, 0, 0.3, 0.1, 0.4, 0, 0.4, 0.1, 0.5, 0, 0.5, 0.1, 0.6, 0, 0.6, 0.1, 0.7, 0, 0.7, 0.1, 0.8, 0, 0.8, 0.1, 0.9, 0, 0.9, 0.1, 1, 0, 1, 0.1, 0.1, 0.2, 0, 0.2, 0.2, 0.2, 0.3, 0.2, 0.4, 0.2, 0.5, 0.2, 0.6, 0.2, 0.7, 0.2, 0.8, 0.2, 0.9, 0.2, 1, 0.2, 0.1, 0.3, 0, 0.3, 0.2, 0.3, 0.3, 0.3, 0.4, 0.3, 0.5, 0.3, 0.6, 0.3, 0.7, 0.3, 0.8, 0.3, 0.9, 0.3, 1, 0.3, 0.1, 0.4, 0, 0.4, 0.2, 0.4, 0.3, 0.4, 0.4, 0.4, 0.5, 0.4, 0.6, 0.4, 0.7, 0.4, 0.8, 0.4, 0.9, 0.4, 1, 0.4, 0.1, 0.5, 0, 0.5, 0.2, 0.5, 0.3, 0.5, 0.4, 0.5, 0.5, 0.5, 0.6, 0.5, 0.7, 0.5, 0.8, 0.5, 0.9, 0.5, 1, 0.5, 0.1, 0.6, 0, 0.6, 0.2, 0.6, 0.3, 0.6, 0.4, 0.6, 0.5, 0.6, 0.6, 0.6, 0.7, 0.6, 0.8, 0.6, 0.9, 0.6, 1, 0.6, 0.1, 0.7, 0, 0.7, 0.2, 0.7, 0.3, 0.7, 0.4, 0.7, 0.5, 0.7, 0.6, 0.7, 0.7, 0.7, 0.8, 0.7, 0.9, 0.7, 1, 0.7, 0.1, 0.8, 0, 0.8, 0.2, 0.8, 0.3, 0.8, 0.4, 0.8, 0.5, 0.8, 0.6, 0.8, 0.7, 0.8, 0.8, 0.8, 0.9, 0.8, 1, 0.8, 0.1, 0.9, 0, 0.9, 0.2, 0.9, 0.3, 0.9, 0.4, 0.9, 0.5, 0.9, 0.6, 0.9, 0.7, 0.9, 0.8, 0.9, 0.9, 0.9, 1, 0.9, 0.1, 1, 0, 1, 0.2, 1, 0.3, 1, 0.4, 1, 0.5, 1, 0.6, 1, 0.7, 1, 0.8, 1, 0.9, 1, 1, 1 ]);
var indices = new Uint16Array([ 0, 1, 2, 0, 2, 3, 1, 4, 5, 1, 5, 2, 4, 6, 7, 4, 7, 5, 6, 8, 9, 6, 9, 7, 8, 10, 11, 8, 11, 9, 10, 12, 13, 10, 13, 11, 12, 14, 15, 12, 15, 13, 14, 16, 17, 14, 17, 15, 16, 18, 19, 16, 19, 17, 18, 20, 21, 18, 21, 19, 3, 2, 22, 3, 22, 23, 2, 5, 24, 2, 24, 22, 5, 7, 25, 5, 25, 24, 7, 9, 26, 7, 26, 25, 9, 11, 27, 9, 27, 26, 11, 13, 28, 11, 28, 27, 13, 15, 29, 13, 29, 28, 15, 17, 30, 15, 30, 29, 17, 19, 31, 17, 31, 30, 19, 21, 32, 19, 32, 31, 23, 22, 33, 23, 33, 34, 22, 24, 35, 22, 35, 33, 24, 25, 36, 24, 36, 35, 25, 26, 37, 25, 37, 36, 26, 27, 38, 26, 38, 37, 27, 28, 39, 27, 39, 38, 28, 29, 40, 28, 40, 39, 29, 30, 41, 29, 41, 40, 30, 31, 42, 30, 42, 41, 31, 32, 43, 31, 43, 42, 34, 33, 44, 34, 44, 45, 33, 35, 46, 33, 46, 44, 35, 36, 47, 35, 47, 46, 36, 37, 48, 36, 48, 47, 37, 38, 49, 37, 49, 48, 38, 39, 50, 38, 50, 49, 39, 40, 51, 39, 51, 50, 40, 41, 52, 40, 52, 51, 41, 42, 53, 41, 53, 52, 42, 43, 54, 42, 54, 53, 45, 44, 55, 45, 55, 56, 44, 46, 57, 44, 57, 55, 46, 47, 58, 46, 58, 57, 47, 48, 59, 47, 59, 58, 48, 49, 60, 48, 60, 59, 49, 50, 61, 49, 61, 60, 50, 51, 62, 50, 62, 61, 51, 52, 63, 51, 63, 62, 52, 53, 64, 52, 64, 63, 53, 54, 65, 53, 65, 64, 56, 55, 66, 56, 66, 67, 55, 57, 68, 55, 68, 66, 57, 58, 69, 57, 69, 68, 58, 59, 70, 58, 70, 69, 59, 60, 71, 59, 71, 70, 60, 61, 72, 60, 72, 71, 61, 62, 73, 61, 73, 72, 62, 63, 74, 62, 74, 73, 63, 64, 75, 63, 75, 74, 64, 65, 76, 64, 76, 75, 67, 66, 77, 67, 77, 78, 66, 68, 79, 66, 79, 77, 68, 69, 80, 68, 80, 79, 69, 70, 81, 69, 81, 80, 70, 71, 82, 70, 82, 81, 71, 72, 83, 71, 83, 82, 72, 73, 84, 72, 84, 83, 73, 74, 85, 73, 85, 84, 74, 75, 86, 74, 86, 85, 75, 76, 87, 75, 87, 86, 78, 77, 88, 78, 88, 89, 77, 79, 90, 77, 90, 88, 79, 80, 91, 79, 91, 90, 80, 81, 92, 80, 92, 91, 81, 82, 93, 81, 93, 92, 82, 83, 94, 82, 94, 93, 83, 84, 95, 83, 95, 94, 84, 85, 96, 84, 96, 95, 85, 86, 97, 85, 97, 96, 86, 87, 98, 86, 98, 97, 89, 88, 99, 89, 99, 100, 88, 90, 101, 88, 101, 99, 90, 91, 102, 90, 102, 101, 91, 92, 103, 91, 103, 102, 92, 93, 104, 92, 104, 103, 93, 94, 105, 93, 105, 104, 94, 95, 106, 94, 106, 105, 95, 96, 107, 95, 107, 106, 96, 97, 108, 96, 108, 107, 97, 98, 109, 97, 109, 108, 100, 99, 110, 100, 110, 111, 99, 101, 112, 99, 112, 110, 101, 102, 113, 101, 113, 112, 102, 103, 114, 102, 114, 113, 103, 104, 115, 103, 115, 114, 104, 105, 116, 104, 116, 115, 105, 106, 117, 105, 117, 116, 106, 107, 118, 106, 118, 117, 107, 108, 119, 107, 119, 118, 108, 109, 120, 108, 120, 119 ]);

