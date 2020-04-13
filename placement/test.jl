import Profile

function test_profile()
    Profile.clear()
    Profile.init(n = 10^7, delay = 0.01)

    open("/tmp/prof.txt", "w") do s
        # I want to ignore all low-cost operations (e.g. <10)
        Profile.print(IOContext(s, :displaysize => (24, 200)), mincount=10)
    end

    # other visualization methods
    #
    # Profile.print()
    # import ProfileView
    # ProfileView.view()
end


