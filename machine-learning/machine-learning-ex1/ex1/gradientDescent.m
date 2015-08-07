function [theta, J_history] = gradientDescent(X, y, theta, alpha, num_iters)
%GRADIENTDESCENT Performs gradient descent to learn theta
%   theta = GRADIENTDESENT(X, y, theta, alpha, num_iters) updates theta by 
%   taking num_iters gradient steps with learning rate alpha

% Initialize some useful values
m = length(y); % number of training examples
J_history = zeros(num_iters, 1);

for iter = 1:num_iters

    % ====================== YOUR CODE HERE ======================
    % Instructions: Perform a single gradient step on the parameter vector
    %               theta. 
    %
    % Hint: While debugging, it can be useful to print out the values
    %       of the cost function (computeCost) and gradient here.
    %

	% the_sum1 = sumfor(X, theta, y, 1);
	% the_sum2 = sumfor(X, theta, y, 2);
	% theta1 = theta(1) - alpha * (1 / m) * the_sum1;
	% theta2 = theta(2) - alpha * (1 / m) * the_sum2;
	% theta = [theta1; theta2];


	h = X * theta - y;
	the_sum = X' * h;
	theta = theta - alpha * (1 / m) * the_sum;

    % ============================================================

    % Save the cost J in every iteration    
    J_history(iter) = computeCost(X, y, theta);

end

end

function the_sum = sumfor(X, theta, y, j)
	the_sum = 0;
	m = length(y);
	for i = 1:m
		the_sum += (hypothesis(X(i,:), theta) - y(i)) * X(i,j);
	end
end
