function res = calculateCost(route,model)    
    city = model.city;
    veh = model.veh;
    maps = model.maps;
    trans = 0;
    
    route = [city+veh route city+veh];
    rlen = 0;
    
    for i = 1:length(route)-1
        rlen = rlen + maps(route(i),route(i+1));
    end
    
    xroute = route(2:length(route));
    loc = find(xroute>model.city);
    last = 1;
    for i = 1:length(loc)
        dot = xroute(last:loc(i)-1);
        cap = model.dmd(dot);
        tlen = 0;
        dot = [city+veh dot city+veh];
        for j = 1:length(dot)-1
           tlen = tlen + maps(dot(j), dot(j+1));
        end
        trans = trans + sum(cap(:))*tlen*0.1/5000/2;
        last = loc(i)+1;
    end
    salary = (rlen/25+sum(model.dmd(:))/5000/6)*15; %salary
    trans = trans +1.86*rlen + 0.1*rlen + 0.2*model.n; %transportation
    res = [rlen salary trans salary+trans];
end