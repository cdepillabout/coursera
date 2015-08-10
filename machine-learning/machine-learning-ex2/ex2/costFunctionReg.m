function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta


h = sigmoid(X * theta);
j_first = (-y) .* log(h);
j_second = (1 - y) .* log(1 - h);
j_each = j_first - j_second;
j_sum = sum(j_each);
J = j_sum / m;

most_theta = theta(2:size(theta));

J = J + lambda / (2 * m) * (most_theta' * most_theta);

grad_sum = X' * (h - y);
grad = grad_sum / m;

grad_0 = grad(1);

theta_other = theta(2:size(theta));

grad_other = grad(2:size(theta)) + (lambda / m) * theta_other;

grad = [grad_0 ; grad_other];


% =============================================================

end
