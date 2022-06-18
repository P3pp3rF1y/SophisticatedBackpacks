package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.tree.LiteralCommandNode;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.synchronization.ArgumentTypeInfo;
import net.minecraft.commands.synchronization.ArgumentTypeInfos;
import net.minecraft.commands.synchronization.SingletonArgumentInfo;
import net.minecraft.core.Registry;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class SBPCommand {
	private static final int OP_LEVEL = 2;
	private static final DeferredRegister<ArgumentTypeInfo<?, ?>> COMMAND_ARGUMENT_TYPES = DeferredRegister.create(Registry.COMMAND_ARGUMENT_TYPE_REGISTRY, SophisticatedBackpacks.MOD_ID);

	private static final RegistryObject<SingletonArgumentInfo<BackpackUUIDArgumentType>> BACKPACK_UUID_COMMAND_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("backpack_uuid", () ->
			ArgumentTypeInfos.registerByClass(BackpackUUIDArgumentType.class, SingletonArgumentInfo.contextFree(BackpackUUIDArgumentType::backpackUuid)));
	private static final RegistryObject<SingletonArgumentInfo<BackpackPlayerArgumentType>> PLAYER_NAME_COMMAND_ARGUMENT_TYPE = COMMAND_ARGUMENT_TYPES.register("player_name", () ->
			ArgumentTypeInfos.registerByClass(BackpackPlayerArgumentType.class, SingletonArgumentInfo.contextFree(BackpackPlayerArgumentType::playerName)));

	private SBPCommand() {}

	public static void init(IEventBus modBus) {
		COMMAND_ARGUMENT_TYPES.register(modBus);

		MinecraftForge.EVENT_BUS.addListener(SBPCommand::registerCommands);
	}

	private static void registerCommands(RegisterCommandsEvent event) {
		CommandDispatcher<CommandSourceStack> dispatcher = event.getDispatcher();
		LiteralCommandNode<CommandSourceStack> mainNode = dispatcher.register(
				Commands.literal("sbp")
						.requires(cs -> cs.hasPermission(OP_LEVEL))
						.then(ListCommand.register())
						.then(GiveCommand.register())
						.then(RemoveNonPlayerCommand.register())
		);
		dispatcher.register(Commands.literal("sophisticatedbackpacks").requires(cs -> cs.hasPermission(OP_LEVEL)).redirect(mainNode));
	}
}
