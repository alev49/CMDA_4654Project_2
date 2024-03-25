kNNclass = function(train, test, y_train, y_test, k, weighted) {
    yhat = c()
    n = nrow(test) # number of test points
    for (i in 1:n) {
        diff = t(train) - test[i, ]
        dist_sq = diag(t(diff) %*% diff) # vector of squared distances
        order = order(dist_sq)

        # picks the x and y from the training set of the nearest k data points
        neighbors.y = y_train[order][1:k]
        neighbors.dist = sqrt(dist_sq[order][1:k])

        classes.near = unique(neighbors.y)
        # computes the predicted class for the test point corresponding to the
        # current iteration of this for loop
        if (weighted) {
            max = -1
            for (j in classes.near) {
                # applies weight function
                total = sum(as.numeric(neighbors.y == j) / neighbors.dist[neighbors.y == j])
                if (total > max) {
                    max = total
                    class = j
                }
            }
        } else {
            max = -1
            for (j in classes.near) {
                total = sum(neighbors.y == j)
                if (total > max) {
                    max = total
                    class = j
                }
            }
        }
        yhat = c(yhat, class)
    }

    accuracy = sum(yhat == y_test) / n

    # calculating confusion matrix entries
    classes = unique(y_test)
    confuse.mat = c()
    for (i in classes) {
        for (j in classes) {
            predicts = yhat[y_test == i]
            confuse.mat = c(confuse.mat, sum(predicts == j))
        }
    }

    confuse.mat = as.data.frame(matrix(confuse.mat, nrow = length(classes)))
    names(confuse.mat) = classes
    rownames(confuse.mat) = paste('predicted', classes)

    return(list('yhat' = yhat, 'accuracy' = accuracy,
            'error.rate' = 1 - accuracy, 'confusion.matrix' = confuse.mat,
            'k' = k))
}

kNNreg = function(train, test, y_train, y_test, k, weighted) {
    yhat = c()
    for (i in 1:nrow(test)) {
        diff = t(train) - test[i, ]
        dist_sq = diag(t(diff) %*% diff) # vector of squared distances
        order = order(dist_sq)

        # picks the x and y from the training set of the nearest k data points
        neighbors.y = y_train[order][1:k]
        neighbors.dist = sqrt(dist_sq[order][1:k])
        # neighbors.dist[neighbors.dist == 0] = min(neighbors.dist[neighbors.dist != 0])
        neighbors.w = c()
        for (i in 1:length(neighbors.dist)) {
            if (neighbors.dist[i] > 0) {
                neighbors.w = c(neighbors.w, 1 / neighbors.dist[i])
            } else {
                neighbors.w = c(neighbors.w, 1 / min(neighbors.dist[neighbors.dist != 0]))
            }
        }

        # neighbors.dist[neighbors.dist == 0] = 0.0001
        neightbors.dist = neighbors.dist * max(neighbors.dist)

        if (weighted) {
            yhat = c(yhat, sum(neighbors.y / neighbors.dist, na.rm = T) * sum(neighbors.dist))
        } else {
            yhat = c(yhat, mean(neighbors.y))
        }
    }

    residuals = y_test - yhat
    sse = as.numeric(t(residuals) %*% residuals)

    return(list('yhat' = yhat, 'residuals' = residuals, 'SSE' = sse, 'k' = k))
}

mykNN <- function(train, test, y_train, y_test, k = 3, weighted = TRUE){
    if (class(test) == 'numeric') {
        test = t(test)
    } else {
        test = as.matrix(test)
    }

    if (is.factor(y_train)) {
        return(kNNclass(train, test, y_train, y_test, k, weighted))
    } else {
        return(kNNreg(train, test, y_train, y_test, k, weighted))
    }
}
