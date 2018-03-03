HTMLWidgets.widget({
  name: 'morpheus',
  type: 'output',
  factory: function (el, width, height) {
    var instance = {};
    return {
      renderValue: function (x) {
        el.innerHTML = '';
        var toDendrogram = function (rootNode) {
          var counter = 0;
          var leafNodes = [];

          function visit(node) {
            var children = node.children;
            if (children !== undefined) {
              var left = children[0];
              var right = children[1];
              left.parent = node;
              right.parent = node;
              visit(left);
              visit(right);
            } else { // leaf node
              node.minIndex = counter;
              node.maxIndex = counter;
              node.index = counter;
              leafNodes.push(node);
              counter++;
            }
          }

          visit(rootNode);
          morpheus.DendrogramUtil.setNodeDepths(rootNode);
          morpheus.DendrogramUtil.setIndices(rootNode);
          return {
            maxHeight: rootNode.height,
            rootNode: rootNode,
            leafNodes: leafNodes,
            nLeafNodes: leafNodes.length
          };
        };

        if (x.columnDendrogram != null) {
          x.columnDendrogram = toDendrogram(x.columnDendrogram);
        }
        if (x.rowDendrogram != null) {
          x.rowDendrogram = toDendrogram(x.rowDendrogram);
        }

        var options = x.options;
        options.el = el;
        options.columnDendrogramField = null;
        options.columnDendrogram = x.columnDendrogram;
        options.rowDendrogramField = null;
        options.rowDendrogram = x.rowDendrogram;
        options.dataset = morpheus.Dataset.fromJSON(x.options.dataset);
        options.width = width;
        options.height = height;
        var heatMap = new morpheus.HeatMap(options);
        instance.heatmap = heatMap;
        el.heatMap = heatMap;
      },
      resize: function (width, height) {
        if (instance.heatmap != null) {
          instance.heatmap.revalidate();
        }
      }
    };
    instance:instance;
  }
});
