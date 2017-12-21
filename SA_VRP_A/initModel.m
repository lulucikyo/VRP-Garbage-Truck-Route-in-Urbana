function model = initModel()
data = csvread('SA.csv') ;

city = length(data) ; % city number except depot
veh = 20 ; % vehicle number
model.city = city;
model.veh = veh;

maps = zeros(city+veh,city+veh);

x0 = 0;
y0 = 0;
model.x0 = x0;
model.y0 = y0;

t = data' ;
x = t(1,:) ;
y = t(2,:) ;
dmd = t(3,:) ;

for k = 1:veh
x = [x x0];
y = [y y0];
end

model.x = x;
model.y = y;
model.dmd = dmd;

n = city+veh;

for i = 1:n
   for j = i:n
      maps(j,i) = (abs(x(i)-x(j))+abs(y(i)-y(j)))*0.621371; 
      maps(i,j) = maps(j,i);
   end
end

model.n = n;
model.maps = maps;

end