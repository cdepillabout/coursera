function [J, grad] = linearRegCostFunction(X, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
%   cost of using theta as the parameter for linear regression to fit the 
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear 
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%

h = X * theta;
hMinusY = h - y;
first = (1 / (2 * m)) * (hMinusY' * hMinusY);

nonFirstTheta = theta(2:end, :);
second = (lambda / (2 * m)) * (nonFirstTheta' * nonFirstTheta);

J = first + second;

thetaFirstRow = theta(1,:);

thetaWithoutFirstRow = theta;
thetaWithoutFirstRow(1,:) = [];

XWithoutFirstColumn = X;
XWithoutFirstColumn(:,1) = [];

gradNoRegularization = (1/m) * (X' * hMinusY);
gradRegularization = (lambda / m) * thetaWithoutFirstRow;

grad = gradNoRegularization  + [0 ; gradRegularization];

% =========================================================================

%grad = grad(:);

end
