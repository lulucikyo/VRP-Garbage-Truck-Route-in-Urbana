function res = isFeasible(route,model)
    loc = find(route>model.city);
    
    len = length(route);
    if route(1)>model.city||route(len)>model.city
        res = 0; return
    end 
    
    
    for i = 2:len
        if route(i)>model.city && route(i-1)>model.city  % not use all vehicles
            res = 0; return
        end
    end
    
    last = 1;
    for i = 1:length(loc)
        dot = route(last:loc(i)-1);
        cap = model.dmd(dot);
        if sum(cap(:))>60000
            res = 0; return
        end %check vehicle capacity
        
        tmp = [model.n dot model.n];
        rlen = 0;
        for j = 1:length(tmp)-1
            rlen = rlen + model.maps(tmp(j), tmp(j+1));
        end
        if (rlen/25+sum(cap(:))/5000/6) > 6
            res = 0; return
        end %check route time constraint
        last = loc(i)+1;
    end
    dot = route(last:len);
    cap = model.dmd(dot);
    if sum(cap(:))>60000
        res = 0; return
    end
    tmp = [model.n dot model.n];
    rlen = 0;
    for j = 1:length(tmp)-1
        rlen = rlen + model.maps(tmp(j), tmp(j+1));
    end
    if (rlen/25+sum(cap(:))/5000/6) > 6
        res = 0; return
    end 
    
    res = 1;
end