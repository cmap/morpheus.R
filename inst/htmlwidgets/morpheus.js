HTMLWidgets.widget({
  name: "morpheus",
  type: "output",
  factory: function(el, width, height) {
    var instance = {};
    return {
      renderValue: function(x) {

        var toDendrogram = function(rootNode) {
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
        var dataset = morpheus.Dataset.fromJSON({
          seriesNames: [x.name],
          seriesDataTypes: ['number'],
          seriesArrays: [x.array],
          rows: x.rows,
          columns: x.columns,
          rowMetadataModel:{vectors:[{name:'id', array:x.rowNames}]},
          columnMetadataModel:{vectors:[{name:'id', array:x.columnNames}]}
        });
       if(x.columnDendrogram) {
         x.columnDendrogram = toDendrogram(x.columnDendrogram);
       }
       if(x.rowDendrogram) {
         x.rowDendrogram = toDendrogram(x.rowDendrogram);
       }
        if(x.rowAnnotations){
          for(var key in x.rowAnnotations) {
            dataset.getRowMetadata().add(key).array = x.rowAnnotations[key];
          }
        }
    
        if(x.columnAnnotations){
          for(var key in x.columnAnnotations) {
            dataset.getColumnMetadata().add(key).array = x.columnAnnotations[key];
          }
        }
        var options = x.options;
        options.el = el;
        options.columnDendrogramField = null;
        options.columnDendrogram = x.columnDendrogram;
        options.rowDendrogramField = null;
        options.rowDendrogram = x.rowDendrogram;
        options.dataset = dataset;
        options.width = width;
        options.height = height;
        var heatMap = new morpheus.HeatMap(options);
        instance.heatmap = heatMap;
      },
      resize: function(width, height) {
       if(instance.heatmap!=null) {
          m.revalidate();
        }
      }
    };
    instance:instance
  }
});
