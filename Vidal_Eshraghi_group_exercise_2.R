kNNclass = function(train, test, y_train, y_test, k, weighted) {
    yhat = c()
    n = nrow(test) # number of test points
    for (i in 1:n) {
        diff = t(train) - test[i, ]
        dist_sq = diag(diff %*% t(diff)) # vector of squared distances
        order = order(dist_sq)

        # picks the x and y from the training set of the nearest k data points
        neighbors.x = x_train[order,][1:k,]
        neighbors.y = y_train[order][1:k]
        neighbors.dist = sqrt(dist_sq[order][1:k])

        classes.near = unique(neighbors.y)
        if (weighted) {
            max = -1
            for (i in classes.near) {
                # applies weight function
                total = sum(neighbors.y == i / neighbor.dist[neighbors.y == i])
                if (total > max) {
                    max = total
                    class = i
                }
            }
        } else {
            max = -1
            for (i in classes.near) {
                total = sum(neighbors.y == i)
                if (total > max) {
                    max = total
                    class = i
                }
            }
        }
        yhat = c(yhat, class)
    }

    accuracy = sum(yhat == y_test) / n

    # calculating confusion matrix entries
    classes = unique(y)
    for (i in classes) {
        for (j in classes) {
            num.guesses = sum(y_hat[y_test == i] == y_test[y_test == i])
            confuse.mat = c(confuse.mat, num.guesses)
        }
    }

    confuse.mat = as.data.frame(matrix(confuse.mat, nrow = length(classes)))
    names(confuse.mat) = classes
    rownames(confuse.mat) = classes

    return(list('yhat' = yhat, 'accuracy' = accuracy,
            'error.rate' = 1 - accuracy, 'confusion.matrix' = confuse.mat,
            'k' = k))
}

kNNreg = function(train, test, y_train, y_test, k, weighted) {
    yhat = c()
    for (i in 1:nrow(test)) {
        diff = t(train) - test[i, ]
        dist_sq = diag(diff %*% t(diff)) # vector of squared distances
        order = order(dist_sq)

        # picks the x and y from the training set of the nearest k data points
        neighbors.x = x_train[order,][1:k,]
        neighbors.y = y_train[order][1:k]
        neighbors.dist = sqrt(dist_sq[order][1:k])

        if (weighted) {
            yhat = c(yhat, sum(neighbors.y / neighbors.dist) * sum(neighbors.dist))
        } else {
            yhat = c(yhat, mean(neighbors.y))
        }
    }

    residuals = ytest - yhat
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
