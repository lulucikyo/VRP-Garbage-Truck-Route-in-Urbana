function model = initModel()
data = csvread('V2.csv') ;

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
id = t(1,:) ;
x = t(2,:) ;
y = t(3,:) ;
dmd = t(4,:) ;

for k = 1:veh
x = [x x0];
y = [y y0];
end

model.id = id;
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